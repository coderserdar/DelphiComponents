unit DeskForm;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, Forms, Menus, DsgnIntf,
  IniFiles;

type
  TDesktopForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAutoSave: Boolean;
    FDeskSection: string;
    FLocked: Boolean;
    FLoadedFromDesktop: Boolean;
    FSaveStateNecessary: Boolean;
    FWindowPlacement: TWindowPlacement;
    FWindowPlacementDirty: Boolean;
    procedure MainFormMadeVisible(Sender: TObject);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  protected
    FLastLoadedBounds: TRect;
    procedure DoMainFormShown; dynamic;
    function LoadDockClients(DeskTop: TMemIniFile; const Section: string;
      DockSite: TWinControl): Boolean; virtual;
    procedure LoadDockStream(DeskTop: TMemIniFile; const Section: string;
      DockSite: TWinControl); virtual;
    procedure SaveDockClients(DeskTop: TMemIniFile; const Section: string;
      DockSite: TWinControl); virtual;
    procedure SaveDockStream(DeskTop: TMemIniFile; const Section: string;
      DockSite: TWinControl); virtual;
    procedure UnlockUpdates; dynamic;
    property SaveStateNecessary: Boolean read FSaveStateNecessary write FSaveStateNecessary;
    procedure ZoomWindow; virtual;
    procedure Repaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditAction(Action: TEditAction); virtual;
    procedure GetEditState(var EditState: TEditState); virtual;
    procedure SaveWindowState(Desktop: TMemIniFile; isProject: Boolean); virtual;
    procedure LoadWindowState(Desktop: TMemIniFile); virtual;
    procedure LockUpdates; dynamic;
    property DeskSection: string read FDeskSection write FDeskSection;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property LoadedFromDesktop: Boolean read FLoadedFromDesktop;
  end;

  TDesktopFormClass = class of TDesktopForm;

  TInitializeForm = procedure(Ident: TComponent);

var
  InitializeForm: TInitializeForm;
  RegisterMenu: procedure (AMenu: TMenu) of object = nil;
  UnregisterMenu: procedure (AMenu: TMenu) of object = nil;
  RegisterMainFormShown: procedure (Event: TNotifyEvent) = nil;
  UnregisterMainFormShown: procedure (Event: TNotifyEvent) = nil;
  GetDockable: function (const DeskSection: string): Boolean = nil;
  LoadedDesktopFormInstances: TStringList;

procedure BeginDesktopUpdate;
procedure EndDesktopUpdate;
function IsDesktopLocked: Boolean;

implementation

uses DeskStrs;

{$R *.DFM}

procedure BeginDesktopUpdate;
begin
end;

procedure EndDesktopUpdate;
begin
end;

function IsDesktopLocked: Boolean;
begin
end;

{ TDesktopForm }

constructor TDesktopForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDesktopForm.EditAction(Action: TEditAction);
var
  Form: TWinControl;
begin
  if (ActiveControl <> nil) then
  begin
    Form := ActiveControl;
    // 'look for a TDesktopForm to delegate EditAction'
    while (Form <> nil) and (not (Form is TDesktopForm)) do
      Form := Form.Parent;
    if (Form <> nil) and (Form <> Self) and (Form is TDesktopForm) then
      TDesktopForm(Form).EditAction (Action);
  end;
end;

procedure TDesktopForm.GetEditState(var EditState: TEditState);
var
  Form: TWinControl;
begin
  if (ActiveControl <> nil) then
  begin
    Form := ActiveControl;
    // 'look for a TDesktopForm to delegate GetEditState'
    while (Form <> nil) and (not (Form is TDesktopForm)) do
      Form := Form.Parent;
    if (Form <> nil) and (Form <> Self) and (Form is TDesktopForm) then
      TDesktopForm(Form).GetEditState (EditState);
  end
  else
    EditState := [];
end;

procedure TDesktopForm.SaveWindowState(Desktop: TMemIniFile; isProject: Boolean);
var
  WindowPlacement: TWindowPlacement;
begin
  { Skip default desktop save if we don't have a Desktop. }
  if Desktop = nil then Exit;
  //IsDocked := HostDockSite <> nil;
  FSaveStateNecessary := True;//IsDocked or (Visible and not IsDocked);
  if FSaveStateNecessary then
    with Desktop do
    begin
      WriteBool(DeskSection, ivCreate, True);
      WriteBool(DeskSection, ivVisible, Visible);
      WriteInteger(DeskSection, ivState, Ord(WindowState));
      WindowPlacement.length := SizeOf(WindowPlacement);
      GetWindowPlacement(Handle, @WindowPlacement);
      with WindowPlacement.rcNormalPosition do
      begin
        WriteInteger(DeskSection, ivLeft, left);
        WriteInteger(DeskSection, ivTop, top);
        WriteInteger(DeskSection, ivWidth, right - left);
        WriteInteger(DeskSection, ivHeight, bottom - top);
        WriteInteger(DeskSection, ivMaxLeft, WindowPlacement.ptMaxPosition.x);
        WriteInteger(DeskSection, ivMaxTop, WindowPlacement.ptMaxPosition.y);
        if WindowState = wsMaximized then
        begin
          WriteInteger(DeskSection, ivMaxWidth, Width);
          WriteInteger(DeskSection, ivMaxHeight, Height);
        end;
      end;
      WriteInteger(DeskSection, ivClientWidth, ClientWidth);
      WriteInteger(DeskSection, ivClientHeight, ClientHeight);
    end;
end;

procedure TDesktopForm.LoadWindowState(Desktop: TMemIniFile);
var
  X, Y, W, H: Integer;
  IsVisible: Boolean;
  WindowState: TWindowState;
begin
  { Skip default desktop load if we don't have a Desktop. }
  if Desktop = nil then Exit;
  if not FLocked and (DesktopUpdateCount > 0) then
    LockUpdates;
  if Desktop.ReadBool(DeskSection, ivCreate, False) then
  begin
    with Desktop do
    begin
      Position := poDesigned;
      IsVisible := ReadBool(DeskSection, ivVisible, False);
      WindowState := TWindowState(ReadInteger(DeskSection, ivState, Ord(wsNormal)));
      X := ReadInteger(DeskSection, ivLeft, Left);
      Y := ReadInteger(DeskSection, ivTop, Top);
      W := ReadInteger(DeskSection, ivWidth, Width);
      H := ReadInteger(DeskSection, ivHeight, Height);

      FLastLoadedBounds := Bounds(X, Y, W, H);
      with FWindowPlacement do
      begin
        length := SizeOf(FWindowPlacement);
        rcNormalPosition.left := X;
        rcNormalPosition.top := Y;
        rcNormalPosition.right := X + W;
        rcNormalPosition.bottom := Y + H;
        ptMaxPosition.x := ReadInteger(DeskSection, ivMaxLeft, -GetSystemMetrics(SM_CXFRAME));
        ptMaxPosition.y := ReadInteger(DeskSection, ivMaxTop, -GetSystemMetrics(SM_CYFRAME));
        if IsVisible then
          case WindowState of
            wsMinimized: showCmd := SW_SHOWMINIMIZED;
            wsMaximized: showCmd := SW_SHOWMAXIMIZED;
            wsNormal: showCmd := SW_NORMAL;
          end
        else
          showCmd := SW_HIDE;
        flags := 0;
      end;
      if FLocked then
      begin
        UpdateBoundsRect(FLastLoadedBounds);
        SetWindowPos(Handle, 0, X, Y, W, H, SWP_NOZORDER or SWP_NOACTIVATE);
        FWindowPlacementDirty := True;
      end
      else
        SetWindowPlacement(Handle, @FWindowPlacement);
      if WindowState = wsMaximized then
      begin
        W := ReadInteger(DeskSection, ivMaxWidth, Width);
        H := ReadInteger(DeskSection, ivMaxHeight, Height);
        SetBounds(FWindowPlacement.ptMaxPosition.x,
          FWindowPlacement.ptMaxPosition.y, W, H);
      end
      else
      begin
        W := ReadInteger(DeskSection, ivClientWidth, -1);
        if W >= 0 then ClientWidth := W;
        H := ReadInteger(DeskSection, ivClientHeight, -1);
        if H >= 0 then ClientHeight := H;
      end;
      Visible := IsVisible;
    end;
    FLoadedFromDesktop := True;
  end;
end;

procedure TDesktopForm.FormCreate(Sender: TObject);
begin
  if Assigned(RegisterMainFormShown) then
    RegisterMainFormShown(MainFormMadeVisible);
  if Assigned(InitializeForm) then
    InitializeForm(Self);
  AutoSave := True;
  if (PopupMenu <> nil) and Assigned(RegisterMenu) then RegisterMenu(PopupMenu);
  if FLocked then LockUpdates;
end;

procedure TDesktopForm.FormDestroy(Sender: TObject);
var
  ListCount: Integer;
begin
  if (PopupMenu <> nil) and Assigned(UnregisterMenu) then UnregisterMenu(PopupMenu);
  if Assigned(UnregisterMainFormShown) then
    UnregisterMainFormShown(MainFormMadeVisible);
  if DesktopForms <> nil then
  begin
    DesktopForms.Remove(Self);
    with DesktopForms.LockList do
    try
      ListCount := Count;
    finally
      DesktopForms.UnlockList;
    end;
    if ListCount = 0 then
    begin
      DesktopForms.Free;
      DesktopForms := nil;
    end;
  end;
end;

type
  TWC = class(TWinControl);

procedure TDesktopForm.LoadDockStream(DeskTop: TMemIniFile;
  const Section: string; DockSite: TWinControl);
var
  DockStream: TMemoryStream;
  StreamText: string;
begin
  StreamText := Desktop.ReadString(DeskSection, Section, '');
  if StreamText <> '' then
  begin
    DockStream := TMemoryStream.Create;
    try
      DockStream.Size := Length(StreamText) div 2;
      DockStream.Seek(0, 0);
      HexToBin(PChar(StreamText), DockStream.Memory, DockStream.Size);
      TWC(DockSite).DockManager.LoadFromStream(DockStream);
    finally
      DockStream.Free;
    end;
  end;
end;

procedure TDesktopForm.SaveDockStream(DeskTop: TMemIniFile;
  const Section: string; DockSite: TWinControl);
var
  DockStream: TMemoryStream;
  StreamText: PChar;
begin
  if TWC(DockSite).UseDockManager and (TWC(DockSite).DockManager <> nil) and
    (DockSite.DockClientCount > 0) then
  begin
    DockStream := TMemoryStream.Create;
    try
      TWC(DockSite).DockManager.SaveToStream(DockStream);
      DockStream.Seek(0, 0);
      StreamText := AllocMem((DockStream.Size * 2) + 1);
      try
        BinToHex(DockStream.Memory, StreamText, DockStream.Size);
        DeskTop.WriteString(DeskSection, Section, StreamText);
      finally
        FreeMem(StreamText);
      end;
    finally
      DockStream.Free;
    end;
  end;
end;

function TDesktopForm.LoadDockClients(DeskTop: TMemIniFile;
  const Section: string; DockSite: TWinControl): Boolean;
var
  ChildList: TStringList;
  Children: string;
  I, Index: Integer;
begin
  Children := Desktop.ReadString(DeskSection, Section, '');
  Result := Children <> '';
  if Result then
  begin
    ChildList := TStringList.Create;
    try
      ExtractStrings([','], [], PChar(Children), ChildList);
      for I := 0 to ChildList.Count - 1 do
      begin
        Index := LoadedDesktopFormInstances.IndexOf(ChildList[I]);
        if (Index >= 0) and (LoadedDesktopFormInstances.Objects[Index] <> nil) then
          (LoadedDesktopFormInstances.Objects[Index] as TDesktopForm).Parent := DockSite;
      end;
    finally
      ChildList.Free;
    end;
  end;
end;

procedure TDesktopForm.SaveDockClients(DeskTop: TMemIniFile;
  const Section: string; DockSite: TWinControl);
var
  I: Integer;
  Children: string;
begin
  for I := 0 to DockSite.DockClientCount - 1 do
  begin
    if Children <> '' then Children := Children + ',';
    Children := Children + (DockSite.DockClients[I] as TDesktopForm).DeskSection;
  end;
  if Children <> '' then
    Desktop.WriteString(DeskSection, Section, Children);
end;

procedure TDesktopForm.MainFormMadeVisible(Sender: TObject);
begin
  DoMainFormShown;
end;

procedure TDesktopForm.DoMainFormShown;
begin
  // This is overridden in descendant forms;
end;

procedure TDesktopForm.LockUpdates;
begin
end;

procedure TDesktopForm.UnlockUpdates;
begin
end;

procedure TDesktopForm.CMShowingChanged(var Message: TMessage);
begin
  if not FLocked then inherited;
end;

procedure TDesktopForm.ZoomWindow;
var
  Form: TCustomForm;
begin
  if HostDockSite <> nil then
    Form := GetParentForm(Self) else
    Form := Self;
  with Form do
    if WindowState = wsNormal then
      WindowState := wsMaximized
    else if WindowState = wsMaximized then
      WindowState := wsNormal;
end;

procedure TDesktopForm.Repaint;
begin
  if not FLocked then inherited;
end;

end.
