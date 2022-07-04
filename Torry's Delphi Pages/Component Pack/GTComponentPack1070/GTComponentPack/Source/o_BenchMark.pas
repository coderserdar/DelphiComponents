{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtBenchMark                                    }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_BenchMark;

interface
uses
    Classes
  ;
type
{------------------------------------------------------------------------------}
  TExec = procedure of Object;
{------------------------------------------------------------------------------}
  TgtBenchMarkResults = class(TPersistent)
  private
    FElapsedTimeInSeconds: Double;
    FElapsedTimeInMinutes: Double;
    FElapsedTimeInMiliSeconds: Double;
    { Private declarations }
  protected
    { Protected declarations }
  public
    procedure Assign(Source:TPersistent);override;  
  public
    { Public declarations }
    property ElapsedTimeInMiliSeconds : Double read FElapsedTimeInMiliSeconds write FElapsedTimeInMiliSeconds;
    property ElapsedTimeInSeconds     : Double read FElapsedTimeInSeconds     write FElapsedTimeInSeconds;
    property ElapsedTimeInMinutes     : Double read FElapsedTimeInMinutes     write FElapsedTimeInMinutes;
  end;
{------------------------------------------------------------------------------}
  TgtBenchMarkInfo = class(TPersistent)
  private
    FProcedureName: string;
    FProcedureDescription: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source : TPersistent);override;    
  published
    { Published declarations}
    property ProcedureName        : string read FProcedureName        write FProcedureName;
    property ProcedureDescription : string read FProcedureDescription write FProcedureDescription;
  end;
{------------------------------------------------------------------------------}
  TgtBenchMark = class(TComponent)
  private
    FActive          : Boolean;
    FOutPutControl   : TComponent;
    FBenchMarkResults: TgtBenchMarkResults;
    FFrequency       : Int64;
    FStartCount      : Int64;
    FStopCount       : Int64;
    FDisplayOutPut   : Boolean;
    FBenchMarkInfo   : TgtBenchMarkInfo;
    procedure SetOutPutControl(const Value: TComponent);
    procedure SetBenchMarkInfo(const Value: TgtBenchMarkInfo);
    procedure SetBenchMarkResults(const Value: TgtBenchMarkResults);
    { Private declarations }
  protected
    { Protected declarations }
    FObjectRunningProcedure : TObject;
    FMethodName             : string;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure DisplayOutPutToOutPutControl; virtual;
    function  BuildDisplayOutPutString:string;virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    procedure Start;virtual;
    procedure Stop;virtual;
    procedure RunProcedure(AObject :TObject ; MethodName : string);
  public
    property BenchMarkResults : TgtBenchMarkResults read FBenchMarkResults write SetBenchMarkResults;
  published
    { Published declarations}
    property Active        : Boolean          read FActive;
    property DisplayOutPut : Boolean          read FDisplayOutPut write FDisplayOutPut default False;
    property OutPutControl : TComponent       read FOutPutControl write SetOutPutControl;
    property BenchMarkInfo : TgtBenchMarkInfo read FBenchMarkInfo write SetBenchMarkInfo;
  end;
{------------------------------------------------------------------------------}


const
  OUTPUT_STRING=
 '<----------------------->'#13+#10+
 'GT BenchMarkComponent    '#13+#10+
 '-----Object Run Info-----'#13+#10+
 'Procedure Run :%s        '#13+#10+
 'Object        :%s        '#13+#10+
 '-----------Info----------'#13+#10+
 'ProcedureName :%s        '#13+#10+
 'ProcedureDescr:%s        '#13+#10+
 '-------Time Results------'#13+#10+
 'MilliSeconds  :%f        '#13+#10+
 'Seconds       :%f        '#13+#10+
 'Minutes       :%f        '#13+#10+
 '';

implementation
uses
   Windows
  ,SysUtils
  ,TypInfo
  ;


{ THBenchMarkInfo }
{------------------------------------------------------------------------------}
procedure TgtBenchMarkInfo.Assign(Source: TPersistent);
begin
  if Source is TgtBenchMarkInfo then
  begin
    Self.ProcedureName        := TgtBenchMarkInfo(Source).ProcedureName;
    Self.ProcedureDescription := TgtBenchMarkInfo(Source).ProcedureDescription;
  end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}


{ THBenchMarkResults }
procedure TgtBenchMarkResults.Assign(Source: TPersistent);
begin
  if Source is TgtBenchMarkResults then
  begin
   Self.ElapsedTimeInMiliSeconds := TgtBenchMarkResults(Source).ElapsedTimeInMiliSeconds;
   Self.ElapsedTimeInSeconds     := TgtBenchMarkResults(Source).ElapsedTimeInSeconds;
   Self.ElapsedTimeInMinutes     := TgtBenchMarkResults(Source).ElapsedTimeInMinutes;
  end
  else
    inherited Assign(Source);

end;
{------------------------------------------------------------------------------}



{ THBenchMark }
{------------------------------------------------------------------------------}
constructor TgtBenchMark.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBenchMarkResults := TgtBenchMarkResults.Create;
  FBenchMarkInfo    := TgtBenchMarkInfo.Create;
  FActive           := False;
  FDisplayOutPut    := False;
end;
{------------------------------------------------------------------------------}
destructor TgtBenchMark.Destroy;
begin
  FreeAndNil(FBenchMarkResults);
  FreeAndNil(FBenchMarkInfo);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtBenchMark.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove  then
    if AComponent = OutPutControl then
      OutPutControl := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtBenchMark.Start;
begin
  FActive := True;
  QueryPerformanceFrequency(FFrequency);
  QueryPerformanceCounter(FStartCount);
end;
{------------------------------------------------------------------------------}
procedure TgtBenchMark.Stop;
begin
  QueryPerformanceCounter(FStopCount);
  FBenchMarkResults.ElapsedTimeInSeconds     := (FStopCount - FStartCount) / FFrequency;
  FBenchMarkResults.ElapsedTimeInMiliSeconds := FBenchMarkResults.ElapsedTimeInSeconds * 1000;
  FBenchMarkResults.ElapsedTimeInMinutes     := FBenchMarkResults.ElapsedTimeInSeconds / 60;
  FActive := False;
  if DisplayOutPut then
    DisplayOutPutToOutPutControl;
end;
{------------------------------------------------------------------------------}
procedure TgtBenchMark.RunProcedure(AObject: TObject; MethodName: string);
var
   Routine: TMethod;
   Exec   : TExec;
begin
   Routine.Data := Pointer(AObject) ;
   Routine.Code := AObject.MethodAddress(MethodName) ;
   if Assigned(Routine.Code) then
   begin
     Exec := TExec(Routine) ;
     Start;
     Exec;
     Stop;
   end;
   FObjectRunningProcedure := AObject;
   FMethodName             := MethodName;
end;
{------------------------------------------------------------------------------}
procedure TgtBenchMark.DisplayOutPutToOutPutControl;
var
  TempStrs : TStrings;
  TempObj  : TObject;
begin
  if Assigned(FOutPutControl) then
  begin
    if (IsPublishedProp(FOutPutControl,'Caption')) then
    begin
      TypInfo.SetStrProp(FOutPutControl,'Caption',BuildDisplayOutPutString);
    end;

    if (IsPublishedProp(FOutPutControl,'Text')) then
    begin
     TypInfo.SetStrProp(FOutPutControl,'Text',BuildDisplayOutPutString);
    end;

    if (IsPublishedProp(FOutPutControl,'Lines')) then
    begin
      try
        TempStrs      := TStringList.Create;
        TempStrs.Text :=  BuildDisplayOutPutString;
        TempObj       := TypInfo.GetObjectProp(FOutPutControl,'Lines');
        if (Assigned(TempObj) and (TempObj.InheritsFrom(TStrings))) then
        begin
          TStrings(TempObj).AddStrings(TempStrs);
        end;
      finally
        FreeAndNil(TempStrs);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtBenchMark.BuildDisplayOutPutString: string;
begin
  if Assigned(FObjectRunningProcedure) then
  begin
    Result := Format(OUTPUT_STRING,[FMethodName,FObjectRunningProcedure.ClassName,FBenchMarkInfo.ProcedureName,FBenchMarkInfo.ProcedureDescription,
                                    FBenchMarkResults.ElapsedTimeInMiliSeconds,FBenchMarkResults.ElapsedTimeInSeconds,
                                    FBenchMarkResults.ElapsedTimeInMinutes]);
  end
  else
    Result := Format(OUTPUT_STRING,['','',FBenchMarkInfo.ProcedureName,FBenchMarkInfo.ProcedureDescription,
                                    FBenchMarkResults.ElapsedTimeInMiliSeconds,FBenchMarkResults.ElapsedTimeInSeconds,
                                    FBenchMarkResults.ElapsedTimeInMinutes]);
end;
{------------------------------------------------------------------------------}


//Getters - Setters \\
{------------------------------------------------------------------------------}
procedure TgtBenchMark.SetOutPutControl(const Value: TComponent);
begin
  if Assigned(FOutPutControl) then
    FOutPutControl.RemoveFreeNotification(Self);

  FOutPutControl := Value;

  if Assigned(FOutPutControl) then
    FOutPutControl.FreeNotification(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtBenchMark.SetBenchMarkInfo(const Value: TgtBenchMarkInfo);
begin
  FBenchMarkInfo.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtBenchMark.SetBenchMarkResults(const Value: TgtBenchMarkResults);
begin
  FBenchMarkResults.Assign(Value);
end;
{------------------------------------------------------------------------------}

















end.
