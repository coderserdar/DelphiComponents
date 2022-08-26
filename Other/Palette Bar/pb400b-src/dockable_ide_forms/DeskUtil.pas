unit DeskUtil;

interface

uses Forms, Classes, Registry, IniFiles, DeskForm;

type
  TLoadDesktopProc = procedure (DeskFile: TCustomIniFile);
  TSaveDesktopProc = procedure (DeskFile: TCustomIniFile; IsProject: Boolean);

function FocusWindow(Window: TForm): TForm;

procedure RegisterDesktopFormClass(AFormClass: TDesktopFormClass; 
  const Section, InstanceName: string);
procedure RegisterDesktopProcs(LoadProc: TLoadDesktopProc; 
  SaveProc: TSaveDesktopProc);
procedure UnregisterDesktopProcs(LoadProc: TLoadDesktopProc; 
  SaveProc: TSaveDesktopProc);
procedure LoadDesktopFormClasses(Desk: TCustomIniFile);
procedure SaveDesktopFormClasses(Desk: TCustomIniFile; IsProject: Boolean);
procedure DoDesktopLoadProcs(Desk: TCustomIniFile);
procedure DoDesktopSaveProcs(Desk: TCustomIniFile; IsProject: Boolean);

var
  IDEIniFile: function: TRegistryIniFile = nil;
  GetFieldAddress: function (const FieldName: string): Pointer = nil;
  RegisterFieldAddress: procedure (const FieldName: string; Address: Pointer) = nil;
  UnregisterFieldAddress: procedure (Address: Pointer) = nil;
  AddMainFormCreatedEvent: procedure (Event: TNotifyEvent) = nil;
  RemoveMainFormCreatedEvent: procedure (Event: TNotifyEvent) = nil;
  AddMainFormShownEvent: procedure (Event: TNotifyEvent) = nil;
  RemoveMainFormShownEvent: procedure (Event: TNotifyEvent) = nil;

implementation

uses SysUtils, DockForm, BaseDock, TabDock, JoinDock, IDEDockCtrls, 
  IDEDockPanel, DeskStrs;

type
  TDesktopFormClassItem = class(TObject)
    FDeskSection: string;
    FInstanceName: string;
    FFormClass: TDesktopFormClass;
    constructor Create(AFormClass: TDesktopFormClass;
      const ADeskSection, AInstanceName: string);
    destructor Destroy; override;
    procedure HideWindow;
    procedure LoadWindow(Desktop: TCustomIniFile);
    procedure SaveWindow(Desktop: TCustomIniFile; IsProject: Boolean);
  end;

  TDesktopProcItem = record
    LoadProc: TLoadDesktopProc;
    SaveProc: TSaveDesktopProc;
  end;
  PDesktopProcItem = ^TDesktopProcItem;

  TDesktopProcArray = array[0..8192] of TDesktopProcItem;
  PDesktopProcArray = ^TDesktopProcArray;

  TDesktopProcs = class(TObject)
  private
    FProcs: TList;
    FCapacity: Integer;
    FProcVec: PDesktopProcArray;
    function GetLoadProc(Index: Integer): TLoadDesktopProc;
    function GetSaveProc(Index: Integer): TSaveDesktopProc;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(LoadProc: TLoadDesktopProc; SaveProc: TSaveDesktopProc);
    function Find(LoadProc: TLoadDesktopProc; SaveProc: TSaveDesktopProc): PDesktopProcItem;
    procedure Remove(LoadProc: TLoadDesktopProc; SaveProc: TSaveDesktopProc);

    procedure DoLoad(DeskFile: TCustomIniFile);
    procedure DoSave(DeskFile: TCustomIniFile; IsProject: Boolean);

    property Count: Integer read GetCount;
    property LoadProcs[Index: Integer]: TLoadDesktopProc read GetLoadProc;
    property SaveProcs[Index: Integer]: TSaveDesktopProc read GetSaveProc;
  end;

var
  DesktopFormClasses: TList;
  DesktopProcs: TDesktopProcs;

{ TDesktopProcs }

constructor TDesktopProcs.Create;
begin
  inherited Create;
  FProcs := TList.Create;
end;

destructor TDesktopProcs.Destroy;
begin
  FreeMem(FProcVec);
  FProcs.Free;
  inherited Destroy;
end;

procedure TDesktopProcs.Add(LoadProc: TLoadDesktopProc; SaveProc: TSaveDesktopProc);
var
  DesktopProcItem: PDesktopProcItem;
  NewCapacity: Integer;
begin
  if Find(LoadProc, SaveProc) = nil then
    while True do
    begin
      if FProcs.Count < FCapacity then
      begin
        DesktopProcItem := Find(nil, nil);
        if DesktopProcItem <> nil then
        begin
          DesktopProcItem.LoadProc := LoadProc;
          DesktopProcItem.SaveProc := SaveProc;
          FProcs.Add(DesktopProcItem);
        end;
        Break;
      end else
      begin
        NewCapacity := FCapacity + 5;
        ReAllocMem(FProcVec, NewCapacity * SizeOf(TDesktopProcItem));
        FillChar(FProcVec[FCapacity], (NewCapacity - FCapacity) * SizeOf(TDesktopProcItem), 0);
        FCapacity := NewCapacity;
      end;
    end;
end;

procedure TDesktopProcs.DoLoad(DeskFile: TCustomIniFile);
var
  I: Integer;
begin
  for I := 0 to FProcs.Count - 1 do
    with PDesktopProcItem(FProcs[I])^ do
      if Assigned(LoadProc) then LoadProc(DeskFile);
end;

procedure TDesktopProcs.DoSave(DeskFile: TCustomIniFile; IsProject: Boolean);
var
  I: Integer;
begin
  for I := 0 to FProcs.Count - 1 do
    with PDesktopProcItem(FProcs[I])^ do
      if Assigned(SaveProc) then SaveProc(DeskFile, IsProject);
end;

function TDesktopProcs.Find(LoadProc: TLoadDesktopProc; SaveProc: TSaveDesktopProc): PDesktopProcItem;
var
  I: Integer;
begin
  for I := 0 to FCapacity - 1 do
    if (@FProcVec[I].LoadProc = @LoadProc) and (@FProcVec[I].SaveProc = @SaveProc) then
    begin
      Result := @FProcVec[I];
      Exit;
    end;
  Result := nil;
end;

function TDesktopProcs.GetCount: Integer;
begin
  Result := FProcs.Count;
end;

function TDesktopProcs.GetLoadProc(Index: Integer): TLoadDesktopProc;
begin
  with PDesktopProcItem(FProcs[Index])^ do
    Result := LoadProc;
end;

function TDesktopProcs.GetSaveProc(Index: Integer): TSaveDesktopProc;
begin
  with PDesktopProcItem(FProcs[Index])^ do
    Result := SaveProc;
end;

procedure TDesktopProcs.Remove(LoadProc: TLoadDesktopProc; SaveProc: TSaveDesktopProc);
begin
  FProcs.Remove(Find(LoadProc, SaveProc));
end;

{ TDesktopFormClassItem }

constructor TDesktopFormClassItem.Create(AFormClass: TDesktopFormClass;
  const ADeskSection, AInstanceName: string);
begin
  inherited Create;
  FFormClass := AFormClass;
  FDeskSection := ADeskSection;
  FInstanceName := AInstanceName;
  DesktopFormClasses.Add(Self);
end;

destructor TDesktopFormClassItem.Destroy;
begin
  DesktopFormClasses.Remove(Self);
  inherited Destroy;
end;

procedure TDesktopFormClassItem.HideWindow;
var
  FormVar: ^TDesktopForm;
begin
  if FInstanceName <> '' then
  begin
    FormVar := GetFieldAddress(FInstanceName);
    if (FormVar <> nil) and (FormVar^ <> nil) then
      FormVar.Hide;
  end;
end;

procedure TDesktopFormClassItem.LoadWindow(Desktop: TCustomIniFile);
var
  FormVar: ^TDesktopForm;
begin
  if FInstanceName <> '' then
  begin
    FormVar := GetFieldAddress(FInstanceName);
    if FormVar <> nil then
    begin
      if DeskTop.ReadBool(FDeskSection, ivCreate, False) then
      begin
        if FormVar^ = nil then FormVar^ := FFormClass.Create(Application);
        FormVar^.LoadWindowState(TMemIniFile(Desktop));
      end
      else if (FormVar^ <> nil) then FormVar^.Hide;
      if FormVar^ <> nil then
        LoadedDesktopFormInstances.AddObject(FormVar^.DeskSection, FormVar^);
    end;
  end;
end;

procedure TDesktopFormClassItem.SaveWindow(Desktop: TCustomIniFile; IsProject: Boolean);
var
  FormVar: ^TDesktopForm;
begin
  FormVar := GetFieldAddress(FInstanceName);
  if (FormVar <> nil) and (FormVar^ <> nil) and FormVar^.AutoSave then
    FormVar^.SaveWindowState(TMemIniFile(Desktop), IsProject);
end;

function FocusWindow(Window: TForm): TForm;
begin
  if Window <> nil then
  begin
    if (Window.HostDockSite <> nil)  and (Window is TDockableForm) then
    begin
      Window.Visible := True;
      if Window.HostDockSite is TTabDockPageControl then
      begin
        TTabDockPageControl(Window.HostDockSite).ShowDockClient(Window);
        Window := Window.HostDockSite.Parent as TTabDockHostForm;
      end
      else if Window.HostDockSite is TJoinDockForm then
        Window := TJoinDockForm(Window.HostDockSite)
      else if Window.HostDockSite is TEditorDockPanel then
        Window := Window.HostDockSite.Owner as TForm;
    end;
    Window.Visible := True;
    if Window.WindowState = wsMinimized then
      Window.WindowState := Forms.wsNormal;
    Window.BringToFront;
  end;
  Result := Window;
end;

procedure RegisterDesktopFormClass(AFormClass: TDesktopFormClass;
  const Section, InstanceName: string);
begin
  if DesktopFormClasses = nil then DesktopFormClasses := TList.Create;
  TDesktopFormClassItem.Create(AFormClass, Section, InstanceName);
end;

procedure RegisterDesktopProcs(LoadProc: TLoadDesktopProc; SaveProc: TSaveDesktopProc);
begin
  if DesktopProcs = nil then
    DesktopProcs := TDesktopProcs.Create;
  DesktopProcs.Add(LoadProc, SaveProc);
end;

procedure UnregisterDesktopProcs(LoadProc: TLoadDesktopProc; SaveProc: TSaveDesktopProc);
begin
  if DesktopProcs <> nil then DesktopProcs.Remove(LoadProc, SaveProc);
end;

procedure LoadDesktopFormClasses(Desk: TCustomIniFile);
var
  I: Integer;
begin
  for I := 0 to DesktopFormClasses.Count - 1 do
    TDesktopFormClassItem(DesktopFormClasses[I]).LoadWindow(Desk);
end;

procedure SaveDesktopFormClasses(Desk: TCustomIniFile; IsProject: Boolean);
var
  I: Integer;
begin
  for I := 0 to DesktopFormClasses.Count - 1 do
    TDesktopFormClassItem(DesktopFormClasses[I]).SaveWindow(Desk, IsProject);
end;

procedure DoDesktopLoadProcs(Desk: TCustomIniFile);
begin
  if Assigned(DesktopProcs) then DesktopProcs.DoLoad(Desk);
end;

procedure DoDesktopSaveProcs(Desk: TCustomIniFile; IsProject: Boolean);
begin
  if Assigned(DesktopProcs) then DesktopProcs.DoSave(Desk, IsProject);
end;

procedure DoneDeskUtil;
var
  I: Integer;
begin
  if DesktopFormClasses <> nil then
    for I := DesktopFormClasses.Count - 1 downto 0 do
      TDesktopFormClassItem(DesktopFormClasses[I]).Free;
  FreeAndNil(DesktopFormClasses);
  FreeAndNil(DesktopProcs);
end;

initialization
finalization
  DoneDeskUtil;
end.
