unit BMDThreadEditors;

interface

{$IF CompilerVersion >= 30} // Delphi 10.0
{$DEFINE D10Up}
{$ENDIF}

{$IF CompilerVersion > 23} // Delphi XE3
{$DEFINE D7}
{$DEFINE D6}
{$DEFINE D9}
{$ENDIF}

{$IFDEF VER230} // Delphi XE2
{$DEFINE D7}
{$DEFINE D6}
{$DEFINE D9}
{$ENDIF}

{$IFDEF VER220} // Delphi XE
{$DEFINE D7}
{$DEFINE D6}
{$DEFINE D9}
{$ENDIF}

{$IFDEF VER210} // Delphi 14.0
{$DEFINE D7}
{$DEFINE D6}
{$DEFINE D9}
{$ENDIF}

{$IFDEF VER200} // Delphi 12.0
{$DEFINE D7}
{$DEFINE D6}
{$DEFINE D9}
{$ENDIF}

{$IFDEF VER190} // Delphi 11.0
{$DEFINE D7}
{$DEFINE D6}
{$DEFINE D9}
{$ENDIF}

{$IFDEF VER180} // Delphi 10.0
{$DEFINE D7}
{$DEFINE D6}
{$DEFINE D9}
{$ENDIF}

{$IFDEF VER170} // Delphi 9.0
{$DEFINE D7}
{$DEFINE D6}
{$DEFINE D9}
{$ENDIF}

{$IFDEF VER150} // Delphi 7.0
{$DEFINE D6}
{$DEFINE D7}
{$ENDIF}

{$IFDEF VER140} // Delphi 6.0
{$DEFINE D6}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFDEF D6}
  DesignIntf,
  DesignEditors,
{$ELSE}
  Dsgnintf,
{$ENDIF}
  Dialogs, TypInfo,
{$IFDEF D9}
  ToolsApi,
{$ELSE}
  ExptIntf,
  EditIntf, ToolIntf,
{$ENDIF}
  BMDThread,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, Clipbrd;

{$IFDEF D6}
type IFormDesigner = IDesigner;
{$ENDIF}

type
{$IFDEF D9}
  TFileNotifyEvent = procedure(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean) of object;
{$ELSE}
  TFileNotifyEvent = procedure(NotifyCode: TFileNotification; const FileName: string; var Cancel: Boolean) of object;
{$ENDIF}
//  TEventNotifyEvent = procedure(NotifyCode: TEventNotification; var Cancel: Boolean) of object;

type
  TEventsEditorNotification = class;

  TSynchroMethodsForm = class(TForm)
    PageControl: TPageControl;
    ThreadSynchroTabSheet: TTabSheet;
    ThreadEventsListView: TListView;
    Panel1: TPanel;
    CreateGroupBox: TGroupBox;
    BtnNoDataEvent: TBitBtn;
    BtnDataEvent: TBitBtn;
    RenameButton1: TBitBtn;
    ShowButton1: TBitBtn;
    Panel3: TPanel;
    CopyCallButton1: TBitBtn;
    GenCallButton1: TBitBtn;
    RefreshButton1: TBitBtn;
    TabSheet2: TTabSheet;
    StdEventsListView: TListView;
    Panel2: TPanel;
    NewStdButton: TBitBtn;
    RenameButton2: TBitBtn;
    ShowButton2: TBitBtn;
    Panel4: TPanel;
    CopyCallButton2: TBitBtn;
    GenCallButton2: TBitBtn;
    RefreshButton2: TBitBtn;
    StatusBar: TStatusBar;
    Timer1: TTimer;
    procedure PageControlChange(Sender: TObject);
    procedure RefreshButton1Click(Sender: TObject);
    procedure BtnNoDataEventClick(Sender: TObject);
    procedure BtnDataEventClick(Sender: TObject);
    procedure RenameButton1Click(Sender: TObject);
    procedure ShowButton1Click(Sender: TObject);
    procedure CopyCallButton1Click(Sender: TObject);
    procedure GenCallButton1Click(Sender: TObject);
    procedure NewStdButtonClick(Sender: TObject);
    procedure ThreadEventsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ThreadEventsListViewEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure ThreadEventsListViewKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
{$IFDEF D9}
    IDENotifierID : Integer;
{$ENDIF}
  protected
    Designer: IFormDesigner;
    UpdateNotifier: TEventsEditorNotification;
    ActiveListView: TListView;
    ThreadEventsList: TStringList;
    StdEventsList: TStringList;
    procedure AddMethodNoDataEntry(const S: string);
    procedure AddMethodDataEntry(const S: string);
    procedure AddStandardNotifyEntry(const S: string);
    function AddMethodNoDataItem(const S: string): TListItem;
    function AddMethodDataItem(const S: string): TListItem;
    function AddStandardNotifyItem(const S: string): TListItem;
    procedure ValidateComponents;
    procedure UpdateView;
    procedure ClearLists;
//    procedure OnFileNotify(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure OnUpdateNotify(Sender : TObject);
    procedure OnCloseNotify(Sender : TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; _Designer: IFormDesigner); reintroduce; overload;
    destructor Destroy; override;
    procedure SetDesigner(_Designer: IFormDesigner);
  end;

  TBMDThreadControlEditor = class(TComponentEditor)
  public
//    destructor Destroy; override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount(): Integer; override;
  end;

  TEmptyEntryPropertyEditor = class(TPropertyEditor)
    function GetValue: string; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{$IFDEF D9}
  TEventsEditorNotification = class( TNotifierObject, IOTANotifier, IOTAIDENotifier )
    FOnUpdateNotify : TNotifyEvent;
    FOnCloseNotify  : TNotifyEvent;
    
    procedure AfterSave;
    { This function is called immediately before the item is saved. This is not
      called for IOTAWizard }
    procedure BeforeSave;
    { The associated item is being destroyed so all references should be dropped.
      Exceptions are ignored. }
    procedure Destroyed;
    { This associated item was modified in some way. This is not called for
      IOTAWizards }
    procedure Modified;

    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    { This function is called immediately before the compiler is invoked.
      Set Cancel to True to cancel the compile }
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    { This procedure is called immediately following a compile.  Succeeded
      will be true if the compile was successful }
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;
{$ELSE}
  TEventsEditorNotification = class(TIAddInNotifier)
  public
    FOnUpdateNotify : TNotifyEvent;
    FOnCloseNotify: TNotifyEvent;
//    FOnEventNotify: TEventNotifyEvent;
    procedure FileNotification(NotifyCode: TFileNotification; const FileName: string; var Cancel: Boolean); override; stdcall;
    procedure EventNotification(NotifyCode: TEventNotification; var Cancel: Boolean); override; stdcall;
  public
    {
     inline __fastcall TEventsEditorNotification (void)
              ToolServices->AddNotifierEx ( this );
    }
  public
    {	inline __fastcall virtual ~TEventsEditorNotification(void)
              ToolServices->RemoveNotifier ( this );
    }
  end;
{$ENDIF}

procedure Register;

implementation

{$R *.DFM}

type
  TThreadEventSruct = class(TObject)
  public
    Parameter: string;
    CallingFormat: string;
  end;

var
  DesignForm: TSynchroMethodsForm;

function GenerateEventName(Designer: IFormDesigner; TamplateName: string): string;
var
  I: Integer;
  GeneratedName: string;
begin
  GeneratedName := TamplateName;
  I := 0;
  repeat
    Inc(I);
    GeneratedName := TamplateName + IntToStr(I);
  until (not Designer.MethodExists(GeneratedName));
  Result := GeneratedName;
end;

function GenerateEvent(Designer: IFormDesigner; TypeID: Pointer; NameTamplate: string): string;
var
  GeneratedName: string;
begin
  //  PPropInfo PropInfo = ::GetPropInfo ( __typeinfo(TBMSynchroEventsEditorHelperClass), EventPropertyName );
  GeneratedName := GenerateEventName(Designer, NameTamplate);
  Designer.CreateMethod(GeneratedName, TypeID);
  Designer.ShowMethod(GeneratedName);
  Result := GeneratedName;
end;

{$IFDEF D9}
function GetCurrentEditView(EditorIntf: IOTASourceEditor): Integer;
var
  EditorFileName: string;
  WindowIterator: HWND;
  Buffer: array[0..255] of Char;
  APos: PChar;
begin
  Assert(EditorIntf <> nil);
  Result := 0;
  if EditorIntf.GetSubViewCount = 1 then
  begin
    // If there is only one edit view, then it is clear that
    // the first one is the current edit view
    Result := 0;
  end
  else
  begin
    EditorFileName := UpperCase(ExtractFileName(EditorIntf.FileName));
    WindowIterator := GetWindow(GetDesktopWindow, GW_CHILD);
    // Iterate over all windows whose owner is the Desktop
    WindowIterator := GetWindow(WindowIterator, GW_HWNDNEXT);
    // If we find a window with the class name "TEditWindow"
    // and the filenames match then this is the currently active
    // edit view
    while( (WindowIterator <> 0) and IsWindow(WindowIterator) ) do
    begin
      if GetClassName(WindowIterator, Buffer, SizeOf(Buffer) - 1) = 0 then
{$IFDEF D10Up}
        RaiseLastOSError;
{$ELSE}
        RaiseLastWin32Error;
{$ENDIF}

      if StrPos(Buffer, 'TEditWindow') <> nil then
      begin
        if( GetWindowText(WindowIterator, Buffer, SizeOf(Buffer) - 1) = 0 ) then
{$IFDEF D10Up}
          RaiseLastOSError;
{$ELSE}
          RaiseLastWin32Error;
{$ENDIF}
        StrUpper(Buffer);
        if StrPos(Buffer, PChar(EditorFileName)) <> nil then
        begin
          // I the case where there are multiple edit windows
          // open the first one you come to in the iteration process
          // should always be the top-most (or most recently active) edit
          // window - JCH
          // Scan window caption from the end; if we started at the
          // beginning, we might run into the colon of C:\MyFile
          APos := StrRScan(Buffer, ':');
          Inc(APos);
          Result := StrToIntDef(StrPas(APos), -1);
          // Subtract 1 since we need 0..GetViewCount-1 rather than 1..GetViewCount
          if Result <> -1 then
            Dec(Result);
          Break;
        end;
      end;
      WindowIterator := GetWindow(WindowIterator, GW_HWNDNEXT);
    end;
  end;
end;

{ TEventsEditorNotification }

procedure TEventsEditorNotification.AfterSave();
begin
  if (Assigned(FOnUpdateNotify)) then
    FOnUpdateNotify(NIL);
    
end;

procedure TEventsEditorNotification.BeforeSave();
begin
  if (Assigned(FOnUpdateNotify)) then
    FOnUpdateNotify(NIL);

end;

procedure TEventsEditorNotification.Destroyed();
begin
  if (Assigned(FOnUpdateNotify)) then
    FOnUpdateNotify(NIL);
    
end;

procedure TEventsEditorNotification.Modified();
begin
  if (Assigned(FOnUpdateNotify)) then
    FOnUpdateNotify(NIL);

end;

procedure TEventsEditorNotification.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin
  if (( NotifyCode = ofnActiveProjectChanged ) or ( NotifyCode = ofnFileClosing )) then
    begin
    if( Assigned(FOnCloseNotify)) then
      FOnCloseNotify( NIL );
      
    end

  else
    if (Assigned(FOnUpdateNotify)) then
      FOnUpdateNotify(NIL);

end;

procedure TEventsEditorNotification.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  if (Assigned(FOnUpdateNotify)) then
    FOnUpdateNotify(NIL);

end;

procedure TEventsEditorNotification.AfterCompile(Succeeded: Boolean);
begin
  if (Assigned(FOnUpdateNotify)) then
    FOnUpdateNotify(NIL);

end;
{$ELSE}
function GetCurrentEditView(EditorIntf: TIEditorInterface): Integer;
var
  EditorFileName: string;
  WindowIterator: HWND;
  Buffer: array[0..255] of Char;
  APos: PChar;
begin
  Assert(EditorIntf <> nil);
  Result := -1;
  if EditorIntf.GetViewCount = 1 then
  begin
    // If there is only one edit view, then it is clear that
    // the first one is the current edit view
    Result := 0;
  end
  else
  begin
    EditorFileName := UpperCase(ExtractFileName(EditorIntf.FileName));
    WindowIterator := GetWindow(GetDesktopWindow, GW_CHILD);
    // Iterate over all windows whose owner is the Desktop
    WindowIterator := GetWindow(WindowIterator, GW_HWNDNEXT);
    // If we find a window with the class name "TEditWindow"
    // and the filenames match then this is the currently active
    // edit view
    while (WindowIterator <> 0) and IsWindow(WindowIterator) do
    begin
      if GetClassName(WindowIterator, Buffer, SizeOf(Buffer) - 1) = 0 then
        RaiseLastWin32Error;
      if StrPos(Buffer, 'TEditWindow') <> nil then
      begin
        if GetWindowText(WindowIterator, Buffer, SizeOf(Buffer) - 1) = 0 then
          RaiseLastWin32Error;
        StrUpper(Buffer);
        if StrPos(Buffer, PChar(EditorFileName)) <> nil then
        begin
          // I the case where there are multiple edit windows
          // open the first one you come to in the iteration process
          // should always be the top-most (or most recently active) edit
          // window - JCH
          // Scan window caption from the end; if we started at the
          // beginning, we might run into the colon of C:\MyFile
          APos := StrRScan(Buffer, ':');
          Inc(APos);
          Result := StrToIntDef(StrPas(APos), -1);
          // Subtract 1 since we need 0..GetViewCount-1 rather than 1..GetViewCount
          if Result <> -1 then
            Dec(Result);
          Break;
        end;
      end;
      WindowIterator := GetWindow(WindowIterator, GW_HWNDNEXT);
    end;
  end;
end;

procedure TEventsEditorNotification.FileNotification(NotifyCode: TFileNotification; const FileName: string; var Cancel: Boolean);
begin
  if ((NotifyCode = fnProjectClosing) or (NotifyCode = fnFileClosing)) then
    begin
    if( Assigned(FOnCloseNotify)) then
      FOnCloseNotify( NIL );
      
    end

  else
    if (Assigned(FOnUpdateNotify)) then
      FOnUpdateNotify(NIL);

end;

procedure TEventsEditorNotification.EventNotification(NotifyCode: TEventNotification; var Cancel: Boolean);
begin
  if (Assigned(FOnUpdateNotify)) then
      FOnUpdateNotify(NIL);
end;
{$ENDIF}

{ TEmptyEntryPropertyEditor }

function TEmptyEntryPropertyEditor.GetValue: string;
begin
  Result := 'Edit...';
end;

procedure TEmptyEntryPropertyEditor.Edit;
begin
  if (DesignForm = nil) then
    DesignForm := TSynchroMethodsForm.Create(nil, Designer);
    
  DesignForm.SetDesigner(Designer);
  DesignForm.Show();
end;

function TEmptyEntryPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TBMDThreadControlEditor }

{
destructor TBMDThreadControlEditor.Destroy;
begin
  if (DesignForm <> nil) then
    begin
    DesignForm.Free;
    DesignForm := nil;
    end;
  inherited;
end;
}

procedure TBMDThreadControlEditor.Edit;
begin
  if (DesignForm = nil) then
    DesignForm := TSynchroMethodsForm.Create(nil, Designer);
    
  DesignForm.SetDesigner(Designer);
  DesignForm.UpdateView();
  DesignForm.Show();
end;

procedure TBMDThreadControlEditor.ExecuteVerb(Index: Integer);
begin
  case (Index) of
    0: GenerateEvent(Designer, GetTypeData(TypeInfo(TBMDThreadSynchroNotifyEvent)), 'SynchroFunc');
    1: GenerateEvent(Designer, GetTypeData(TypeInfo(TBMDThreadSynchroDataNotifyEvent)), 'SynchroFuncData');
  else
    Edit();
  end;
end;

function TBMDThreadControlEditor.GetVerb(Index: Integer): string;
begin
  case (Index) of
    0: Result := 'New S.M. No Data';
    1: Result := 'New S.M. + Data';
  else
    Result := 'Edit S.M. ...';
  end;
end;

function TBMDThreadControlEditor.GetVerbCount(): Integer;
begin
  Result := 3;
end;

{ TSynchroMethodsForm }

constructor TSynchroMethodsForm.Create(AOwner: TComponent; _Designer: IFormDesigner);
{$IFDEF D9}
var
  Services : IOTAServices;
{$ENDIF}
begin
  inherited Create(AOwner);
  Designer := _Designer;
  UpdateNotifier := TEventsEditorNotification.Create;
{$IFDEF D9}
  Services := (BorlandIDEServices as IOTAServices);
  IDENotifierID := Services.AddNotifier( UpdateNotifier );
{$ELSE}
  ToolServices.AddNotifierEx(UpdateNotifier);
{$ENDIF}
//  UpdateNotifier.FOnFileNotify := OnFileNotify;
  UpdateNotifier.FOnUpdateNotify := OnUpdateNotify;
  UpdateNotifier.FOnCloseNotify := OnCloseNotify;
  ThreadEventsList := TStringList.Create;
  StdEventsList := TStringList.Create;
  ValidateComponents;
end;

destructor TSynchroMethodsForm.Destroy;
{$IFDEF D9}
var
  Services : IOTAServices;
{$ENDIF}
begin
  ThreadEventsListView.OnChange := nil;
  StdEventsListView.OnChange := nil;
  ClearLists;
  StdEventsList.Free;
  ThreadEventsList.Free;
{$IFDEF D9}
  Services := (BorlandIDEServices as IOTAServices);
  Services.RemoveNotifier( IDENotifierID );
{$ELSE}
  ToolServices.RemoveNotifier(UpdateNotifier);
  UpdateNotifier.Free;
{$ENDIF}
  inherited;
end;

procedure TSynchroMethodsForm.PageControlChange(Sender: TObject);
begin
  ValidateComponents;
end;

procedure TSynchroMethodsForm.ClearLists;
var
  I: Integer;
begin
  for I := 0 to ThreadEventsList.Count - 1 do
    ThreadEventsList.Objects[I].Free;
  // delete ((TThreadEventSruct *)ThreadEventsList.Objects [ I ] );
  ThreadEventsList.Clear();
  StdEventsList.Clear();
end;

procedure TSynchroMethodsForm.SetDesigner(_Designer: IFormDesigner);
begin
  Designer := _Designer;
  if (Designer <> nil) then
    UpdateView();
end;

procedure TSynchroMethodsForm.OnUpdateNotify(Sender : TObject);
begin
  DesignForm.UpdateView;
end;

procedure TSynchroMethodsForm.OnCloseNotify(Sender : TObject);
begin
  Designer := NIL;
  Hide();
end;

procedure TSynchroMethodsForm.UpdateView;
var
  OldOnChange: TLVChangeEvent;
  Updated: Boolean;
  I: Integer;
  J: Integer;
  Found: Boolean;
  Item: TListItem;
  Struct: TThreadEventSruct;
begin
  if (Designer = nil) then
    Exit;
  ClearLists;
  //  if ( PageControl.ActivePage == ThreadSynchroTabSheet )
  //  PropInfo := GetPropInfo ( __typeinfo(TBMSynchroEventsEditorHelperClass), "ThreadNotifyEventEmptyEvent" );
  Designer.GetMethods(GetTypeData(TypeInfo(TBMDThreadSynchroNotifyEvent)), AddMethodNoDataEntry);
  //  PropInfo = GetPropInfo ( __typeinfo(TBMSynchroEventsEditorHelperClass), "ThreadDataNotifyEventEmptyEvent" );
  Designer.GetMethods(GetTypeData(TypeInfo(TBMDThreadSynchroDataNotifyEvent)), AddMethodDataEntry);
  //  PropInfo = ::GetPropInfo ( __typeinfo(TButton), "OnClick" );
  Designer.GetMethods(GetTypeData(TypeInfo(TNotifyEvent)), AddStandardNotifyEntry);
  //  ThreadEventsListView.Items.Clear ();
  OldOnChange := ThreadEventsListView.OnChange;
  ThreadEventsListView.OnChange := nil;
  Updated := false;
  I := 0;
  while I < ThreadEventsListView.Items.Count do
  begin
    Found := false;
    for J := 0 to ThreadEventsList.Count - 1 do
    begin
      if (ThreadEventsListView.Items.Item[I].Caption = ThreadEventsList.Strings[J]) then
      begin
        if (ThreadEventsListView.Items.Item[I].SubItems.Strings[0] = TThreadEventSruct(ThreadEventsList.Objects[J]).Parameter) then
        begin
          Found := true;
          // delete ((TThreadEventSruct *)ThreadEventsList.Objects [ J ]);
          ThreadEventsList.Objects[J].Free;
          ThreadEventsList.Delete(J);
          break;
        end;
      end;
    end;
    if (not Found) then
    begin
      if (not Updated) then
        ThreadEventsListView.Items.BeginUpdate();
      Updated := true;
      ThreadEventsListView.Items.Item[I].Free;
      Dec(I);
    end;
    Inc(I);
  end;
  for I := 0 to ThreadEventsList.Count - 1 do
  begin
    Item := ThreadEventsListView.Items.Add();
    Item.Caption := ThreadEventsList.Strings[I];
    Struct := (ThreadEventsList.Objects[I]) as TThreadEventSruct;
    Item.SubItems.Add(Struct.Parameter);
    Item.SubItems.Add(Struct.CallingFormat);
  end;
  ThreadEventsListView.OnChange := OldOnChange;
  if (Updated) then
    ThreadEventsListView.Items.EndUpdate();
  Updated := false;
  //  StdEventsListView.Items.Clear ();
  OldOnChange := StdEventsListView.OnChange;
  StdEventsListView.OnChange := nil;
  I := 0;
  while I < StdEventsListView.Items.Count do //  for ( int I := 0; I < StdEventsListView.Items.Count; I ++ )
  begin
    Found := false;
    for J := 0 to StdEventsList.Count - 1 do
    begin
      if (StdEventsListView.Items.Item[I].Caption = StdEventsList.Strings[J]) then
      begin
        Found := true;
        StdEventsList.Delete(J);
        break;
      end;
    end;
    if (not Found) then
    begin
      if (not Updated) then
        StdEventsListView.Items.BeginUpdate();
      Updated := true;
      StdEventsListView.Items.Item[I].Free;
      Dec(I);
    end;
    Inc(I);
  end;
  for I := 0 to StdEventsList.Count - 1 do
  begin
    Item := StdEventsListView.Items.Add();
    Item.Caption := StdEventsList.Strings[I];
    Item.SubItems.Add('TObject *Sender');
    Item.SubItems.Add('');
  end;
  StdEventsListView.OnChange := OldOnChange;
  if (Updated) then
    StdEventsListView.Items.EndUpdate();
  ValidateComponents;
end;

procedure TSynchroMethodsForm.ValidateComponents;
begin
  if (PageControl.ActivePage = ThreadSynchroTabSheet) then
    ActiveListView := ThreadEventsListView
  else
    ActiveListView := StdEventsListView;
  ShowButton1.Enabled := (ActiveListView.ItemFocused <> nil);
  ShowButton2.Enabled := (ActiveListView.ItemFocused <> nil);
  RenameButton1.Enabled := ShowButton1.Enabled;
  RenameButton2.Enabled := ShowButton1.Enabled;
  GenCallButton1.Enabled := ShowButton1.Enabled;
  GenCallButton2.Enabled := ShowButton1.Enabled;
  CopyCallButton1.Enabled := ShowButton1.Enabled;
  CopyCallButton2.Enabled := ShowButton1.Enabled;
  //  CreateGroupBox.Visible := ( TabControl.TabIndex = 0 );
  //  RenameButton.Visible := CreateGroupBox.Visible;
  if (ActiveListView.ItemFocused <> nil) then
    StatusBar.SimpleText := 'Thread.Synchronize( ' + ActiveListView.ItemFocused.Caption + ActiveListView.ItemFocused.SubItems.Strings[1] + ' );'
  else
    StatusBar.SimpleText := '';
end;

procedure TSynchroMethodsForm.RefreshButton1Click(Sender: TObject);
begin
  UpdateView();
end;

procedure TSynchroMethodsForm.BtnNoDataEventClick(Sender: TObject);
begin
  if (Designer = nil) then
    Exit;
  AddMethodNoDataItem(GenerateEvent(Designer, GetTypeData(TypeInfo(TBMDThreadSynchroNotifyEvent)), 'SynchroFunc')).EditCaption();
  FormResize(Sender);
end;

procedure TSynchroMethodsForm.BtnDataEventClick(Sender: TObject);
begin
  if (Designer = nil) then
    Exit;
  AddMethodDataItem(GenerateEvent(Designer, GetTypeData(TypeInfo(TBMDThreadSynchroDataNotifyEvent)), 'SynchroFuncData')).EditCaption();
  FormResize(Sender);
end;

procedure TSynchroMethodsForm.RenameButton1Click(Sender: TObject);
begin
  if (ActiveListView.ItemFocused <> nil) then
  begin
    ActiveListView.ItemFocused.EditCaption();
  end;
end;

procedure TSynchroMethodsForm.ShowButton1Click(Sender: TObject);
begin
  if (Designer = nil) then
    Exit;
  if (ActiveListView.ItemFocused <> nil) then
  begin
    Designer.ShowMethod(ActiveListView.ItemFocused.Caption);
  end;
  Show();
end;

procedure TSynchroMethodsForm.CopyCallButton1Click(Sender: TObject);
begin
  Clipboard.AsText := StatusBar.SimpleText;
end;

{$IFDEF D9}
procedure TSynchroMethodsForm.GenCallButton1Click(Sender: TObject);
var
  I: Integer;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  EditorIntf: IOTAEditor;
  Editor: IOTASourceEditor;
  CurrentView: Integer;
  View: IOTAEditView;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
  OldBlockType: TOTABlockType;
  Writer: IOTAEditWriter;
begin
//  ModIntf := ToolServices.GetModuleInterface(ToolServices.GetCurrentFile());
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Module := ModuleServices.CurrentModule;

  if (Module = nil) then
  begin
    ShowMessage(
      'Please select the editor bofore pressing this button.'#13 +
      'For now the code will go to the clipboard.'#13 +
      'You can paste wherever you want.'
      );
    Clipboard().AsText := StatusBar.SimpleText;
  end
  else
  begin
//    Module.Show();
//    ModIntf.ShowSource();

    // Get the interface to the source editor.
    for I := 0 to Module.GetModuleFileCount-1 do
    begin
      EditorIntf := Module.GetModuleFileEditor(I);
      if( EditorIntf.QueryInterface(IOTASourceEditor, Editor) = S_OK ) then
        Break;
        
    end;

    // If the file is not a source file, Editor is nil.
    if Editor = nil then
      Exit;

    begin
      if (Editor.EditViewCount > 0) then
      begin
        Editor.Show();
        CurrentView := GetCurrentEditView(Editor);
        View := Editor.EditViews[ CurrentView ];
        if (View <> nil) then
        begin
          try
            begin
              EditPos := View.CursorPos;
              View.ConvertPos(true, EditPos, CharPos);
              OldBlockType := Editor.BlockType;
              Editor.BlockType := btInclusive;
              Editor.BlockStart := CharPos;
              Editor.BlockAfter := CharPos;
              Editor.BlockVisible := True;
              if (MessageDlg('Insert the code at this location ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
              begin
                Writer := Editor.CreateUndoableWriter();
                try
                  begin
                    Writer.CopyTo(View.CharPosToPos(CharPos));
                    if ((CharPos.CharIndex = 0) and (EditPos.Col > 0)) then
                      for I := 0 to EditPos.Col - 2 do
                        Writer.Insert(' ');
                    Writer.Insert(PAnsiChar(AnsiString(StatusBar.SimpleText)));
                    Writer.Insert(#13#10);
                    for I := 0 to EditPos.Col - 1 do
                      Writer.Insert(' ');
                    Writer.CopyTo(MaxLongint);
                  end;
                finally
                  Writer := NIL;
                end;
                Editor.Show();
                Inc(EditPos.Line);
                View.CursorPos := EditPos;
              end;
              Editor.BlockType := OldBlockType;
              Editor.BlockStart := CharPos;
              Editor.BlockAfter := CharPos;
              Editor.BlockVisible := False;
            end;
          except;
          end;
//          View.Free();
        end;
      end;
//      EditorIntf.Free();
    end;
//    ModIntf.Free();
  end;
end;

{$ELSE}

procedure TSynchroMethodsForm.GenCallButton1Click(Sender: TObject);
var
  I: Integer;
  ModIntf: TIModuleInterface;
  EditorIntf: TIEditorInterface;
  CurrentView: Integer;
  View: TIEditView;
  CharPos: TCharPos;
  EditPos: TEditPos;
  OldBlockType: TBlockType;
  Writer: TIEditWriter;
begin
  ModIntf := ToolServices.GetModuleInterface(ToolServices.GetCurrentFile());
  if (ModIntf = nil) then
  begin
    ShowMessage(
      'Please select the editor bofore pressing this button.'#13 +
      'For now the code will go to the clipboard.'#13 +
      'You can paste wherever you want.'
      );
    Clipboard().AsText := StatusBar.SimpleText;
  end
  else
  begin
    ModIntf.ShowSource();
    EditorIntf := ModIntf.GetEditorInterface();
    if (EditorIntf <> nil) then
    begin
      if (EditorIntf.GetViewCount() > 0) then
      begin
        CurrentView := GetCurrentEditView(EditorIntf);
        View := EditorIntf.GetView(CurrentView);
        if (View <> nil) then
        begin
          try
            begin
              EditPos := View.CursorPos;
              View.ConvertPos(true, EditPos, CharPos);
              OldBlockType := EditorIntf.BlockType;
              EditorIntf.BlockType := btInclusive;
              EditorIntf.BlockStart := CharPos;
              EditorIntf.BlockAfter := CharPos;
              EditorIntf.BlockVisible := true;
              if (MessageDlg('Insert the code at this location ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
              begin
                Writer := EditorIntf.CreateUndoableWriter();
                try
                  begin
                    Writer.CopyTo(View.CharPosToPos(CharPos));
                    if ((CharPos.CharIndex = 0) and (EditPos.Col > 0)) then
                      for I := 0 to EditPos.Col - 2 do
                        Writer.Insert(' ');
                    Writer.Insert(PChar(StatusBar.SimpleText));
                    Writer.Insert(#13#10);
                    for I := 0 to EditPos.Col - 1 do
                      Writer.Insert(' ');
                    Writer.CopyTo(MaxLongint);
                  end;
                finally
                  Writer.Free();
                end;
                ModIntf.ShowSource();
                Inc(EditPos.Line);
                View.CursorPos := EditPos;
              end;
              EditorIntf.BlockType := OldBlockType;
              EditorIntf.BlockStart := CharPos;
              EditorIntf.BlockAfter := CharPos;
              EditorIntf.BlockVisible := false;
            end;
          except;
          end;
          View.Free();
        end;
      end;
      EditorIntf.Free();
    end;
    ModIntf.Free();
  end;
end;

{$ENDIF}
procedure TSynchroMethodsForm.NewStdButtonClick(Sender: TObject);
begin
  if (Designer = nil) then
    Exit;
  AddStandardNotifyItem(GenerateEvent(Designer, GetTypeData(TypeInfo(TNotifyEvent)), 'NotifyEvent')).EditCaption();
  FormResize(Sender);
end;

procedure TSynchroMethodsForm.ThreadEventsListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  ValidateComponents();
end;

procedure TSynchroMethodsForm.ThreadEventsListViewEdited(Sender: TObject;
  Item: TListItem; var S: string);
begin
  if (Designer = nil) then
    Exit;
  if (Item.Caption <> S) then
  begin
    if (Designer.MethodExists(S)) then
    begin
      S := GenerateEventName(Designer, S);
    end;
    Designer.RenameMethod(Item.Caption, S);
  end;
end;

procedure TSynchroMethodsForm.ThreadEventsListViewKeyPress(Sender: TObject;
  var Key: Char);
begin
  if ((Key = #13) or (Key = ' ')) then
    ShowButton1Click(Sender);
end;

procedure TSynchroMethodsForm.FormResize(Sender: TObject);
begin
  ThreadEventsListView.Column[1].Width := ThreadEventsListView.ClientWidth - ThreadEventsListView.Column[0].Width;
  StdEventsListView.Column[1].Width := StdEventsListView.ClientWidth - StdEventsListView.Column[0].Width;
end;

procedure TSynchroMethodsForm.FormShow(Sender: TObject);
begin
  FormResize(Sender);
  ValidateComponents;
end;

procedure TSynchroMethodsForm.AddMethodNoDataEntry(const S: string);
var
  Struct: TThreadEventSruct;
begin
  Struct := TThreadEventSruct.Create;
  Struct.Parameter := 'Thread : TBMExecuteThread';
  ThreadEventsList.AddObject(S, Struct);
end;

procedure TSynchroMethodsForm.AddMethodDataEntry(const S: string);
var
  Struct: TThreadEventSruct;
begin
  Struct := TThreadEventSruct.Create;
  Struct.Parameter := 'Thread : TBMExecuteThread; var Data : Pointer';
  Struct.CallingFormat := ', [%Data to pass here]';
  ThreadEventsList.AddObject(S, Struct);
end;

procedure TSynchroMethodsForm.AddStandardNotifyEntry(const S: string);
begin
  StdEventsList.Add(S);
end;

function TSynchroMethodsForm.AddMethodNoDataItem(const S: string): TListItem;
var
  Item: TListItem;
begin
  Item := ThreadEventsListView.Items.Add();
  Item.Caption := S;
  Item.SubItems.Add('Thread : TBMExecuteThread');
  Item.SubItems.Add('');
  Result := Item;
end;

function TSynchroMethodsForm.AddMethodDataItem(const S: string): TListItem;
var
  Item: TListItem;
begin
  Item := ThreadEventsListView.Items.Add();
  Item.Caption := S;
  Item.SubItems.Add('Thread : TBMExecuteThread; var Data : Pointer');
  Item.SubItems.Add(', @ [%Data to pass here]');
  Result := Item;
end;

function TSynchroMethodsForm.AddStandardNotifyItem(const S: string): TListItem;
var
  Item: TListItem;
begin
  Item := StdEventsListView.Items.Add();
  Item.Caption := S;
  Item.SubItems.Add('Sender : TObject');
  Item.SubItems.Add('');
  Result := Item;
end;

procedure TSynchroMethodsForm.Timer1Timer(Sender: TObject);
begin
  UpdateView;
  ValidateComponents;
end;

procedure Register;
begin
  RegisterComponents('BMitov', [TBMDThread, TBMDThreadGroup]);
  RegisterPropertyEditor(TypeInfo(TPersistent), TBMDThread, 'SynchroMethods', TEmptyEntryPropertyEditor);
  RegisterComponentEditor(TBMDThread, TBMDThreadControlEditor);
end;

initialization
  DesignForm := nil;
finalization
  if (DesignForm <> nil) then
    begin
    DesignForm.Free;
    DesignForm := nil;
    end;
    
end.

