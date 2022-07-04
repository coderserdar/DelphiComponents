{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_ProcessListView;

interface
uses
   Classes
  ,Controls
  ,ComCtrls
  ,Menus
  ,o_ProcessManager
  ,o_GTTimer
  ;

type
{------------------------------------------------------------------------------}
  TgtProcessListView = class;
{------------------------------------------------------------------------------}
  TgtProcessInfoThread = class(TThread)
  private
    { Private declarations }
    FProcessListView : TgtProcessListView;
  protected
    { Protected declarations }
    FProcessManager : TgtProcessManager;
    procedure UpdateUI;
  public
    { Public declarations }
    procedure Execute;override;
  public
    constructor Create(AProcessListView:TgtProcessListView);
    destructor  Destroy;override;
  end;
{------------------------------------------------------------------------------}
  TgtProcessListView = class(TCustomListView)
  private
    FRefreshInterval: Cardinal;
    { Private declarations }
  protected
    { Protected declarations }
    FProcessInfoThread : TgtProcessInfoThread;
    FInitialized       : Boolean;
    procedure CreateColumns;
    procedure Initialize;
  protected
    procedure SetParent(AParent: TWinControl);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property RefreshInterval : Cardinal read FRefreshInterval write FRefreshInterval;
    property Align;
  end;
{------------------------------------------------------------------------------}

implementation
uses
   SysUtils
  ,Windows
  ;
const
  ProcessColumns : array [0..22] of string =
   (
      'ExeName'
     ,'Size'
     ,'Usage'
     ,'Memory Usage'
     ,'PID'
     ,'HeapID'
     ,'ModuleID'
     ,'Threads'
     ,'PPID'
     ,'ClassBase'
     ,'Flags'
     ,'MemoryInfo'
     ,'Priority'
     ,'RecordSize'
     ,'PageFaultCount'
     ,'PeakWorkingSetSize'
     ,'WorkingSetSize'
     ,'QuotaPeakPagedPoolUsage'
     ,'QuotaPagedPoolUsage'
     ,'QuotaPeakNonPagedPoolUsage'
     ,'QuotaNonPagedPoolUsage'
     ,'PagefileUsage'
     ,'PeakPagefileUsage'
   );

{ TgtProcessListView }
{------------------------------------------------------------------------------}
constructor TgtProcessListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle          := vsReport;
  FProcessInfoThread := TgtProcessInfoThread.Create(Self);
  FRefreshInterval   := 5000;
  FInitialized       := False;
end;
{------------------------------------------------------------------------------}
destructor TgtProcessListView.Destroy;
begin
  if Assigned(FProcessInfoThread) then
  begin
    FProcessInfoThread.Terminate;
    FProcessInfoThread := nil;
  end;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtProcessListView.CreateColumns;
var
  i : integer;
begin
  if (Assigned(Columns) and (Columns.Count = 0))  then
  begin
    for i:=Low(ProcessColumns) to High(ProcessColumns) do
    begin
      with Columns.Add do
      begin
        AutoSize := False;
        Caption  := ProcessColumns[i];
        Width    := 80;
        {if i > 6 then
          Width := 0;}
      end;
    end;
  end;
  FProcessInfoThread.Resume;
end;
{------------------------------------------------------------------------------}
procedure TgtProcessListView.Initialize;
begin
  CreateColumns;
  FInitialized := True;
end;
{------------------------------------------------------------------------------}




//Getters - Setters\\
{------------------------------------------------------------------------------}
procedure TgtProcessListView.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
//  if Assigned(AParent) then
  if not FInitialized then
    Initialize;
end;
{------------------------------------------------------------------------------}



{ TgtProcessInfoThread }
{------------------------------------------------------------------------------}
constructor TgtProcessInfoThread.Create(AProcessListView: TgtProcessListView);
begin
  inherited Create(True);
  FProcessListView := AProcessListView;
end;
{------------------------------------------------------------------------------}
destructor TgtProcessInfoThread.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtProcessInfoThread.Execute;
begin
  while not Terminated do
  begin
    WaitForSingleObject(Handle,FProcessListView.RefreshInterval);
    FProcessManager := TgtProcessManager.Create(nil);
    try
      FProcessManager.UpdateRunningProcessList;
      Synchronize(UPdateUI);
    finally
      FreeAndNil(FProcessManager);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtProcessInfoThread.UpdateUI;
var
  j : Integer;
begin
  FProcessListView.Items.BeginUpdate;
  try
    Self.Suspend;
    FProcessListView.Items.Clear;
    for j:= 0 to Pred(FProcessManager.RunningProcessList.Count) do
        with FProcessListView.Items.Add do
        begin
          Caption := FProcessManager.RunningProcesses[j].ExeName;
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].Size));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].Usage));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryUse));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].PID));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].HeapID));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].ModuleID));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].Threads));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].PPID));

          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].ClassBase));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].Flags));
{          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].Priority));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].RecordSize));}
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.PageFaultCount));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.PeakWorkingSetSize));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.WorkingSetSize));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.QuotaPeakPagedPoolUsage));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.QuotaPagedPoolUsage));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.QuotaPeakNonPagedPoolUsage));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.QuotaNonPagedPoolUsage));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.PagefileUsage));
          SubItems.Add(FloatToStr(FProcessManager.RunningProcesses[j].MemoryInfo.PeakPagefileUsage));
        end;
  finally
    FProcessListView.Items.EndUpdate;
    Self.Resume;
  end;
end;
{------------------------------------------------------------------------------}

end.

