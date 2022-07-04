{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       o_GTButtons                                     }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTButtons;

interface

uses
   Classes
  ,Buttons
  ,Controls
  ,Graphics
  ,Messages
  ,Windows
  ,DB
  ;
  {$I ../../GTDIRECTIVES.inc}
type
{------------------------------------------------------------------------------}
  TgtButtonAction = (
                       gbaNone
                      ,gbaSelectFolder
                      ,gbaSelectPrinter
                      ,gbaSelectComputer
                      ,gbaSelectFile
                      ,gdaSelectPicture
                      ,gbaSelectColor
                      ,gbaSelectFont
//                      ,gbaCalculator
                      ,gbaControlPanelApplet
                    );

{------------------------------------------------------------------------------}
  TgtButtonActionResult = class(TPersistent)
  private
    FCalculatorResult: Double;
    FColorResult: Integer;
    FPrinterResult: string;
    FFileResult: string;
    FFolderResult: string;
    FPictureResult: string;
    FFontResult: TFont;
    FComputerResult: string;
    FExecuted: Boolean;
    procedure SetFontResult(const Value: TFont);
    { Private declarations }
  protected
    { Protected declarations }
    FControlPanelResult : Integer;
  public
    { Public declarations }
    constructor Create;
    destructor  Destroy;override;
  published
    { Published declarations}
    property FolderResult      : string  read FFolderResult      write FFolderResult;
    property ComputerResult    : string  read FComputerResult    write FComputerResult;
    property PrinterResult     : string  read FPrinterResult     write FPrinterResult;
    property FileResult        : string  read FFileResult        write FFileResult;
    property PictureResult     : string  read FPictureResult     write FPictureResult;
    property ColorResult       : Integer read FColorResult       write FColorResult;
    property FontResult        : TFont   read FFontResult        write SetFontResult;
    property CalculatorResult  : Double  read FCalculatorResult  write FCalculatorResult;
    property Executed          : Boolean read FExecuted          write FExecuted;
  end;
{------------------------------------------------------------------------------}
  TgtCustomDialogButton = class(TSpeedButton)
  private
    FAction          : TgtButtonAction;
    FAssosiate       : TWinControl;
    FActionResult    : TgtButtonActionResult;
    FDialogCaption   : string;
    FDialogInitialDir: string;
    FDialogFilter    : string;
    FOnAfterExecute  : TNotifyEvent;
    procedure SetAssosiate(const Value: TWinControl);
    { Private declarations }
  protected
    { Protected declarations }
    procedure RePositionToAssosiate;
    procedure InternalOnClick(Sender : TObject);
    procedure InternalOnAssosiateResize(Sender : TObject);
    procedure Notification(AComponent: TComponent;Operation: TOperation);override;
    function  Execute(ButtonAction : TgtButtonAction):TgtButtonActionResult;virtual;abstract;
  protected
    FOldAssosiateResizeEvent : TNotifyEvent;
    FOldAssosiateWndProc     : TWndMethod;
    FHandle                  : Cardinal;
    procedure WindowProc(var Message  : TMessage);dynamic;
    procedure AssosiateWindowProc(var Message : TMessage);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property ActionResult : TgtButtonActionResult read FActionResult;
  published
    { Published declarations}
    property Action                           : TgtButtonAction  read FAction                          write FAction;
    property Assosiate                        : TWinControl      read FAssosiate                       write SetAssosiate;
    property DialogCaption                    : string           read FDialogCaption                   write FDialogCaption;
    property DialogInitialDir                 : string           read FDialogInitialDir                write FDialogInitialDir;
    property DialogFilter                     : string           read FDialogFilter                    write FDialogFilter;
    property OnAfterExecute                   : TNotifyEvent     read FOnAfterExecute                  write FOnAfterExecute;
  end;
{------------------------------------------------------------------------------}
  TgtDialogButton = class(TgtCustomDialogButton)
  private
    { Private declarations }
  protected
    { Protected declarations }
    function BrowseDialog(const Title: string; const Flag: integer): string;virtual;
    function Execute(ButtonAction : TgtButtonAction):TgtButtonActionResult;override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property OnAfterExecute;
  end;
{------------------------------------------------------------------------------}
 TgtControlPanelApplet = (
                            cpaNone
                           ,cpaAccesibility
                           ,cpaAddRemoveProgramms
                           ,cpaDesktop
                           ,cpaFirewall
                           ,cpaHardwareWizard
                           ,cpaIESettings
                           ,cpaRegionalSettings
                           ,cpaJoystick
                           ,cpaMouse
                           ,cpaSound
                           ,cpaNetWorkConnections
                           ,cpaWANWizard
                           ,cpaUsers
                           ,cpaODBC
                           ,cpaPowerSettings
                           ,cpaSystem
                           ,cpaPhoneAndModem
                           ,cpaDateTime
                           ,cpaSecurityCenter
                           ,cpaWindowsUpdates
                          );
{------------------------------------------------------------------------------}
  TgtControlPanelDialog = class(TgtCustomDialogButton)
  private
    FControlPanelApplet: TgtControlPanelApplet;
    { Private declarations }
  protected
    { Protected declarations }
    FWindowsSystemDir : string;
    procedure RunControlPanelApplet(FileName :string);
    function Execute(ButtonAction : TgtButtonAction):TgtButtonActionResult;override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property ControlPanelApplet : TgtControlPanelApplet read FControlPanelApplet write FControlPanelApplet;
    property OnAfterExecute;
  end;
{------------------------------------------------------------------------------}
  TgtDBButtonAction = (
                         dbaNone
                        ,dbaFirst
                        ,dbaLast
                        ,dbaNext
                        ,dbaPrior
                        ,dbaInsert
                        ,dbaAppend
                        ,dbaEdit
                        ,dbaPost
                        ,dbaCancel
                        ,dbaLocate
                        ,dbaRefresh
                        ,dbaClose
                        ,dbaOpen
                        );
{------------------------------------------------------------------------------}
  TgtDBButton = class(TSpeedButton)
  private
    FDataSet: TDataSet;
    FDataSource: TDataSource;
    FOnBeforeAction: TNotifyEvent;
    FDBAction: TgtDBButtonAction;
    FOnAfterAction: TNotifyEvent;
    FDBActionLocateValue: string;
    FDBActionLocateOptions: TLocateOptions;
    FDBActionLocateKey: string;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetDataSource(const Value: TDataSource);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification(AComponent : TComponent ; Operation : TOperation);override;
    procedure PerformAction;virtual;
    function  ProperDataSet : TDataSet;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Click;override;
  published
    { Published declarations}
    property DataSource             : TDataSource         read FDataSource             write SetDataSource;
    property DataSet                : TDataSet            read FDataSet                write SetDataSet;
    property DBAction               : TgtDBButtonAction   read FDBAction               write FDBAction;
    property DBActionLocateKeyField : string              read FDBActionLocateKey      write FDBActionLocateKey;
    property DBActionLocateValue    : string              read FDBActionLocateValue    write FDBActionLocateValue;
    property DBActionLocateOptions  : TLocateOptions      read FDBActionLocateOptions  write FDBActionLocateOptions;
  published
    property OnBeforeAction : TNotifyEvent    read FOnBeforeAction write FOnBeforeAction;
    property OnAfterAction  : TNotifyEvent    read FOnAfterAction  write FOnAfterAction;
  end;
{------------------------------------------------------------------------------}



type
  _TControl = class(TControl);

implementation
uses
   ShellApi
  ,ShlObj
  ,Dialogs
  ,ExtDlgs
  ,SysUtils
  ;

  //Standard Control Panel Applets\\
const
  StdCpls : array [0..19] of string =
                        ( 'access.cpl'//Accecibility for disabled
                         ,'appwiz.cpl'//Add remove programms
                         ,'desk.cpl'//Desktop
                         ,'firewall.cpl'//Firewall
                         ,'hdwwiz.cpl'//Hardware Wizard
                         ,'inetcpl.cpl'//Internet Explorer Settings
                         ,'intl.cpl'//Regional Settings
                         ,'joy.cpl'//Joystick Settings
                         ,'main.cpl'//Mouse Settings
                         ,'mmsys.cpl'//Sound Settings
                         ,'ncpa.cpl'//Network Cpnnections Explorer view
                         ,'netsetup.cpl'//WAN Setup Wizard
                         ,'nusrmgr.cpl'//Users Management
                         ,'odbccp32.cpl'//ODBC Settings
                         ,'powercfg.cpl'//Power Settings
                         ,'sysdm.cpl'//System Settings
                         ,'telephon.cpl'//Phone and Modem Settings
                         ,'timedate.cpl'//System Time and Date
                         ,'wscui.cpl'//Security Center Windows XP SP2 and after
                         ,'wuaucpl.cpl'//Windows Updates
                         );
{$IFDEF DELPHI6}
   const
      BIF_NEWDIALOGSTYLE     = $0040;
{$ENDIF}

{ TgtButtonActionResult }
{------------------------------------------------------------------------------}
constructor TgtButtonActionResult.Create;
begin
  FFontResult := TFont.Create;
end;
{------------------------------------------------------------------------------}
destructor TgtButtonActionResult.Destroy;
begin
  FFontResult.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtButtonActionResult.SetFontResult(const Value: TFont);
begin
  FFontResult.Assign(Value);
end;
{------------------------------------------------------------------------------}




{ TgtCustomDialogButton }
{------------------------------------------------------------------------------}
constructor TgtCustomDialogButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.OnClick := InternalOnClick;
end;
{------------------------------------------------------------------------------}
destructor TgtCustomDialogButton.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomDialogButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FAssosiate then
      Assosiate := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtCustomDialogButton.WindowProc(var Message: TMessage);
begin
end;
{------------------------------------------------------------------------------}
procedure TgtCustomDialogButton.AssosiateWindowProc(var Message: TMessage);
begin
  FOldAssosiateWndProc(Message);
  case Message.Msg of
    WM_MOVE : RePositionToAssosiate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomDialogButton.InternalOnAssosiateResize(Sender: TObject);
begin
  RePositionToAssosiate;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomDialogButton.InternalOnClick(Sender: TObject);
begin
 FActionResult := Execute(Self.Action);
 if Assigned(FAssosiate) then
  if FAssosiate.CanFocus then
      FAssosiate.SetFocus;
 if Assigned(FActionResult) then
   if FActionResult.Executed then
     if Assigned(FOnAfterExecute) then
      FOnAfterExecute(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtCustomDialogButton.RePositionToAssosiate;
begin
  if Assigned(FAssosiate) then
  begin
    if Assigned(FOldAssosiateResizeEvent) then
      FOldAssosiateResizeEvent(FAssosiate);
    Self.Height := FAssosiate.Height;
    Self.Left   := FAssosiate.Left + FAssosiate.Width + 2;
    Self.Top    := FAssosiate.Top;
  end;
end;
{------------------------------------------------------------------------------}




{------------------------------------------------------------------------------}
procedure TgtCustomDialogButton.SetAssosiate(const Value: TWinControl);
begin
  if Assigned(FAssosiate) then
  begin
    FAssosiate.RemoveFreeNotification(Self);
    if Assigned(FOldAssosiateResizeEvent) then
      _TControl(FAssosiate).OnResize := FOldAssosiateResizeEvent;
    if Assigned(FOldAssosiateWndProc) then
      FAssosiate.WindowProc := FOldAssosiateWndProc;
  end;

  FAssosiate := Value;

  if Assigned(FAssosiate) then
  begin
    FAssosiate.FreeNotification(Self);
    if Assigned(_TControl(FAssosiate).OnResize) then
      FOldAssosiateResizeEvent :=  _TControl(FAssosiate).OnResize;
    _TControl(FAssosiate).OnResize := InternalOnAssosiateResize;
    RePositionToAssosiate;
    FOldAssosiateWndProc  := FAssosiate.WindowProc;
    FAssosiate.WindowProc := AssosiateWindowProc;
  end;
end;
{------------------------------------------------------------------------------}



{ TgtDialogButton }
{------------------------------------------------------------------------------}
constructor TgtDialogButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{------------------------------------------------------------------------------}
destructor TgtDialogButton.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
function TgtDialogButton.Execute(ButtonAction: TgtButtonAction): TgtButtonActionResult;
var
  ComDlg : TCommonDialog;
begin
  Result := nil;
  if ButtonAction <> gbaNone then
  begin
    Result := TgtButtonActionResult.Create;
    Result.Executed := False;
  end;

  case ButtonAction of
    gbaNone            :;
    gbaSelectFolder    :
      begin
         Result.FolderResult    := BrowseDialog(FDialogCaption,BIF_RETURNONLYFSDIRS   or BIF_NEWDIALOGSTYLE or BIF_EDITBOX);
         Result.Executed        := Length(Result.FolderResult) > 0;
      end;
    gbaSelectPrinter   :
      begin
       Result.PrinterResult   := BrowseDialog(FDialogCaption,BIF_BROWSEFORPRINTER );
       Result.Executed        := Length(Result.PrinterResult) > 0;
      end;
    gbaSelectComputer  :
      begin
       Result.ComputerResult  := BrowseDialog(FDialogCaption,BIF_BROWSEFORCOMPUTER);
       Result.Executed        := Length(Result.ComputerResult) > 0;
      end;
    gbaSelectFile      :
      begin
       Result.FileResult      := BrowseDialog(FDialogCaption,BIF_BROWSEINCLUDEFILES);
       Result.Executed        := Length(Result.FileResult) > 0;
      end;
    gdaSelectPicture   :
      begin
         try
          ComDlg := TOpenPictureDialog.Create(nil);
          TOpenPictureDialog(ComDlg).InitialDir := FDialogInitialDir;
          TOpenPictureDialog(ComDlg).Filter     := FDialogFilter;
          if TOpenPictureDialog(ComDlg).Execute then
          begin
            Result.PictureResult                  := TOpenPictureDialog(ComDlg).FileName;
            Result.Executed                       := True;
          end;
         finally
          FreeAndNil(ComDlg);
         end;
      end;
    gbaSelectColor     :
      begin
         try
          ComDlg := TColorDialog.Create(nil);
          if TColorDialog(ComDlg).Execute then
          begin
            Result.ColorResult    := TColorDialog(ComDlg).Color;
            Result.Executed       := True;
          end;
         finally
          FreeAndNil(ComDlg);
         end;
      end;
    gbaSelectFont      :
      begin
         try
          ComDlg := TFontDialog.Create(nil);
          if TFontDialog(ComDlg).Execute then
          begin
            Result.FontResult    := TFontDialog(ComDlg).Font;
            Result.Executed      := True;
          end;
         finally
          FreeAndNil(ComDlg);
         end;
      end;
//    gbaCalculator      :;
  end;
end;
{------------------------------------------------------------------------------}
function TgtDialogButton.BrowseDialog(const Title: string;const Flag: integer): string;
{Taken from http://delphi.about.com/od/windowsshellapi/l/aa070400a.htm}
var
  lpItemID    : PItemIDList;
  BrowseInfo  : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of char;
  TempPath    : array[0..MAX_PATH] of char;
begin
  Result:='';
  try
    FHandle      := Classes.AllocateHWnd(WindowProc);
    FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
    with BrowseInfo do begin
      hwndOwner      := FHandle;
      pszDisplayName := @DisplayName;
      lpszTitle      := PChar(Title);
      ulFlags        := Flag;
    end;
    lpItemID := SHBrowseForFolder(BrowseInfo);
    if lpItemId <> nil then
    begin
      SHGetPathFromIDList(lpItemID, TempPath);
      Result := TempPath;
      GlobalFreePtr(lpItemID);
    end;
  finally
    Classes.DeallocateHWnd(FHandle);
  end;
end;
{------------------------------------------------------------------------------}





{ TgtControlPanelDialog }
{------------------------------------------------------------------------------}
constructor TgtControlPanelDialog.Create(AOwner: TComponent);
var 
  dir: array [0..MAX_PATH] of Char; 
begin
  inherited Create(AOwner);
  GetSystemDirectory(dir, MAX_PATH);
  FWindowsSystemDir := IncludeTrailingPathDelimiter(StrPas(Dir));
  Self.Action       := gbaControlPanelApplet;
end;
{------------------------------------------------------------------------------}
destructor TgtControlPanelDialog.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
function TgtControlPanelDialog.Execute(ButtonAction: TgtButtonAction): TgtButtonActionResult;
begin
  Result := nil;
  if Self.ControlPanelApplet <> cpaNone then
  begin
    Result := TgtButtonActionResult.Create;
    Result.FControlPanelResult := 0;
    RunControlPanelApplet(StdCpls[Ord(ControlPanelApplet)-1]);
    Result.Executed            := True;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtControlPanelDialog.RunControlPanelApplet(FileName: string);
begin
  if FileExists(FWindowsSystemDir + FileName) then
    ShellExecute(HWND(nil),'open','control.exe',PChar(FileName),'',0);
end;
{------------------------------------------------------------------------------}

{ TgtDBButton }
{------------------------------------------------------------------------------}
constructor TgtDBButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Width  := 41;
  Self.Height := 41;
end;
{------------------------------------------------------------------------------}
destructor TgtDBButton.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBButton.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FDataSet then
      DataSet := nil;
    if AComponent = FDataSource then
      DataSource := nil;
  end;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtDBButton.Click;
begin
  inherited;
  if Assigned(FOnBeforeAction) then
    FOnBeforeAction(Self);
  PerformAction;
  if Assigned(FOnAfterAction) then
    FOnAfterAction(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtDBButton.PerformAction;
begin
  case FDBAction of
    dbaNone   : ;
    dbaFirst  :
      begin
        if ProperDataSet.State in [dsBrowse] then
          ProperDataSet.First;
      end;
    dbaLast   :
      begin
        if ProperDataSet.State in [dsBrowse] then
          ProperDataSet.Last;
      end;
    dbaNext   :
      begin
        if ProperDataSet.State in [dsBrowse] then
          ProperDataSet.Next;
      end;
    dbaPrior  :
      begin
        if ProperDataSet.State in [dsBrowse] then
          ProperDataSet.Prior;
      end;
    dbaInsert :
      begin
        if ProperDataSet.State in [dsBrowse] then
          ProperDataSet.Insert;
      end;
    dbaAppend :
      begin
        if ProperDataSet.State in [dsBrowse] then
          ProperDataSet.Append;
      end;
    dbaEdit   :
      begin
        if ProperDataSet.State in [dsBrowse] then
          ProperDataSet.Edit;
      end;
    dbaPost   :
      begin
        if ProperDataSet.State in [dsInsert,dsEdit] then
          ProperDataSet.Post;
      end;
    dbaCancel :
      if ProperDataSet.State in [dsInsert,dsEdit] then
          ProperDataSet.Cancel;
    dbaLocate :
      begin
        if ProperDataSet.State in [dsBrowse] then
          ProperDataSet.Locate(DBActionLocateKeyField,DBActionLocateValue,DBActionLocateOptions)
      end;
    dbaRefresh:
      begin
       if ProperDataSet.State in [dsBrowse] then
        ProperDataSet.Refresh;
      end;
    dbaClose  :
      begin
        ProperDataSet.Active := False;
      end;
    dbaOpen   :
      begin
        ProperDataSet.Active := True;
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtDBButton.ProperDataSet: TDataSet;
begin
  Result := nil;
  if Assigned(DataSource) then
    if Assigned(DataSource) then
      Result := DataSource.DataSet;
  if Assigned(DataSet) then
    Result := DataSet;
  if Result = nil  then
    raise Exception.Create('DataSet or DataSource not set!');
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtDBButton.SetDataSet(const Value: TDataSet);
begin
  if Assigned(FDataSet) then
  begin
    FDataSet.RemoveFreeNotification(Self);
  end;

  FDataSet := Value;

  if Assigned(FDataSet) then
  begin
    FDataSet.FreeNotification(Self);
    if Assigned(DataSource) then
      DataSource := nil;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBButton.SetDataSource(const Value: TDataSource);
begin
  if Assigned(FDataSource) then
  begin
    FDataSource.RemoveFreeNotification(Self);
  end;

  FDataSource := Value;

  if Assigned(FDataSource) then
  begin
    FDataSource.FreeNotification(Self);
    if Assigned(DataSet) then
      DataSet := nil;
  end;
end;
{------------------------------------------------------------------------------}

end.
