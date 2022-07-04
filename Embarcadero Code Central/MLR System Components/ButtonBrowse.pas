unit ButtonBrowse;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ShlObj, ActiveX;

type
  {TBrowseEvent = procedure (Sender :TObject) of object;}
  TBrowseEvent = TNotifyEvent;
	TBrowseForFolderDialog = class
  private
    bi              :TBROWSEINFO;
    str             :array[0..MAX_PATH] of Char;
    pIDListItem     :PItemIDList;
    FPath           :string;
    FTitle          :string;
    FHandle         :HWND;
    FOnInitialized  :TBrowseEvent;
    FOnChanged      :TBrowseEvent;
    procedure SetTitle(Title: String);
    function  GetTitle: String;
    procedure SetPath(const Value: string);
    function BrowseCallback(Wnd :HWND; uMsg :UINT; lParam :LPARAM) :Integer;
  public
    function Execute			  :Boolean;
    property Handle         :HWND read FHandle;
		property Title				  :string read GetTitle write SetTitle;
		property Path				    :string read FPath write SetPath;
    property OnInitialized  :TBrowseEvent read FOnInitialized write FOnInitialized;
    property OnChanged      :TBrowseEvent read FOnChanged write FOnChanged;
	end;

	TButtonBrowse = class(TButton)
	private
    FRemovePath: Boolean;
		{ Private declarations }
	protected
		{ Protected declarations }
		FInformation						:string;
		FForFolder							:Boolean;
		FOpenDialog							:TOpenDialog;
		FCancelled							:Boolean;
		FAssociation						:TWinControl;
		FBrowseForFolderDialog	:TBrowseForFolderDialog;
		FAppendSlash						:Boolean;
		FExternal								:Boolean;
	public
		{ Public declarations }
		constructor Create(AOwner :TComponent); override;
		destructor Destroy; override;
    procedure Click; override;
		procedure Loaded; override;
		procedure	Notification(AComponent :TComponent; Operation :TOperation); override;
		property Cancelled :Boolean read FCancelled;
	published
		{ Published declarations }
		property ForFolder		:Boolean read FForFolder write FForFolder default True;
		property Information	:string read FInformation write FInformation;
		property OpenDialog		:TOpenDialog read FOpenDialog write FOpenDialog;
		property Association	:TWinControl read FAssociation write FAssociation;
		property AppendSlash	:Boolean read FAppendSlash write FAppendSlash default False;
    property RemovePath   :Boolean read FRemovePath write FRemovePath default False;
	end;

  procedure Register;

implementation

function GeneralBrowseCallback(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  Result := TBrowseForFolderDialog(lpData).BrowseCallback(Wnd, uMsg, lParam);
end;

function PIDLToString(P :PItemIDList) :string;
var
  FindData      :^WIN32_FIND_DATA;
  DesktopFolder :IShellFolder;
  ShellMalloc   :IMalloc;
  R             :HResult;
begin
  if not Succeeded(SHGetDesktopFolder(DesktopFolder)) then
    raise Exception.Create('Cannot create desktop interface');
  if not Succeeded(SHGetMalloc(ShellMalloc)) then
    raise Exception.Create('Cannot create shell memory allocator');
  FindData := ShellMalloc.Alloc(sizeof(WIN32_FIND_DATA));
  if not Assigned(FindData) then
    raise EOutOfMemory.Create('Cannot allocate size for description');
  try
    R := SHGetDataFromIDList(
      DesktopFolder,            { Interface to parent folder. }
      P,                        { Pointer to ID list. }
      SHGDFIL_FINDDATA,         { Request data file information. }
      FindData,                 { Pointer to buffer to receive information. }
      sizeof(WIN32_FIND_DATA)); { Size of the buffer passed in. }
    case R of
      NOERROR:      Result := FindData.cFileName;
      E_INVALIDARG: Result := 'Invalid';
      else          Result := '';
    end;
  finally
    ShellMalloc.Free(FindData);
  end;
end;

procedure TButtonBrowse.Click;
var P	:PChar;
begin
	if FForFolder then begin
    if (FInformation = '') and Assigned(Association) then begin
      P := StrAlloc(355);
      SendMessage(FAssociation.Handle, WM_GETTEXT, 355, Longint(P));
      FInformation := StrPas(P);
      StrDispose(P);
    end;
    
    if FInformation <> '' then
      FBrowseForFolderDialog.Path := FInformation;

		if FBrowseForFolderDialog.Execute then begin
			FInformation := FBrowseForFolderDialog.Path;
			FCancelled := False;
		end else begin
			FInformation := '';
			FCancelled := True;
		end;
	end else begin
    if not Assigned(FOpenDialog) then exit;
		if FOpenDialog.Execute then begin
      if FRemovePath then
        FInformation := ExtractFileName(FOpenDialog.FileName)
      else
  			FInformation := FOpenDialog.FileName;
			FCancelled := False;
		end else begin
			FInformation := '';
			FCancelled := True;
		end;
	end;

	if not FCancelled then begin
		if FAppendSlash then begin
			if FInformation[Length(FInformation)] = '\' then
				FInformation := FInformation + '\';
		end;
		if FAssociation <> nil then begin
			P := StrAlloc(Length(FInformation)+1);
			StrPCopy(P, FInformation);
			SendMessage(FAssociation.Handle, WM_SETTEXT, 0, Integer(P));
			StrDispose(P);
		end;
	end;
  
  inherited Click;
end;

constructor TButtonBrowse.Create(AOwner :TComponent);
begin
	inherited Create(AOwner);
	FBrowseForFolderDialog	:= TBrowseForFolderDialog.Create;
	FForFolder							:= True;
	FAppendSlash						:= False;
	Caption									:= 'Browse...';
	FExternal								:= False;
	FOpenDialog							:= nil;
end;

destructor TButtonBrowse.Destroy;
begin
	FBrowseForFolderDialog.Free;
	inherited Destroy;
end;

procedure TButtonBrowse.Loaded;
begin
	inherited Loaded;
	if FOpenDialog = nil then ForFolder	:= True;
end;

procedure Register;
begin
	RegisterComponents('MLR Sysco', [TButtonBrowse]);
end;

procedure TBrowseForFolderDialog.SetTitle(Title: String);
begin
  FTitle  := Title;
end;

function  TBrowseForFolderDialog.GetTitle: String;
begin
  Result := FTitle;
end;

function TBrowseForFolderDialog.Execute: Boolean;
begin
	bi.hwndOwner			:= GetActiveWindow;
  bi.pidlRoot       := nil;
	bi.pszDisplayName	:= @str;
  bi.lpszTitle      := PChar(FTitle);
	bi.ulFlags				:= BIF_RETURNONLYFSDIRS;
	bi.lpfn 					:= GeneralBrowseCallback;
  bi.lParam         := Integer(Self);
	pIDListItem 			:= SHBrowseForFolder(bi);
	if pIDListItem <> nil then begin
    SetLength(FPath, MAX_PATH);
		SHGetPathFromIDList(pIDListItem, PChar(FPath));
		CoTaskMemFree(pIDListItem);
    SetLength(FPath, Pos(#0, FPath) - 1);
		Result := True;
	end else
		Result := False;
end;

procedure TButtonBrowse.Notification(AComponent :TComponent; Operation :TOperation);
begin
	inherited Notification(AComponent, Operation);
	if (AComponent = FAssociation) and (Operation = opRemove) then FAssociation := nil;
	if (AComponent = FOpenDialog) and (Operation = opRemove) then FOpenDialog := nil;
end;

procedure TBrowseForFolderDialog.SetPath(const Value: string);
begin
  FPath := Value;
end;

function TBrowseForFolderDialog.BrowseCallback(Wnd: HWND; uMsg: UINT;
  lParam: LPARAM): Integer;
var
  pidlSelection :PItemIDList;
  DesktopFolder :IShellFolder;
  WidePath      :WideString;
  pchEaten      :Cardinal;
  dwAttributes  :Cardinal;
  ppidl         :PItemIDList;
  R             :HResult;
begin
  FHandle := Wnd;
  if uMsg = BFFM_INITIALIZED then begin
    if FPath <> '' then begin
      SHGetDesktopFolder(DesktopFolder);
      WidePath      := FPath;
      pchEaten      := 0;
      dwAttributes  := 0;
      R := DesktopFolder.ParseDisplayName(Wnd, nil, PWideChar(WidePath),
        pchEaten, ppidl, dwAttributes);
      if Succeeded(R) then
        SendMessage(FHandle, BFFM_SETSELECTION, 0, Integer(ppidl));
    end;
    if Assigned(FOnInitialized) then FOnInitialized(Self);
  end else begin
    pidlSelection := PItemIDList(lParam);
    StrPCopy(str, PIDLToString(pidlSelection));
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
  Result := 0;
end;

end.
