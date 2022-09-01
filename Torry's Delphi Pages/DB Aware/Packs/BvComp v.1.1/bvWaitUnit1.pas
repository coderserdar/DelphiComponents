unit bvWaitUnit1;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Buttons,ExtCtrls,ComCtrls,
{$else}
  QGraphics,
  QControls,
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
  Qt,
  QDialogs,
  QComCtrls,
{$endif}

  Classes,
  bvCursorUnit, math,bvLocalization;

const IndicatorPosOnEdge:boolean=true;

type
  TMyWait= class(TComponent)
  private
    thWaitForm :tForm;

//    OutStepNN,OutNNCount:integer;
    WaitC:TWaitCursor;

    function GetProgressbarPosition:integer;
    procedure SetProgressBarPosition(Value:integer);

    function GetProgress:TProgressBar;

    procedure SetDopText(Text:String);
    function GetDopText:string;
  public

    constructor Create(MSG:String='';X:integer=-1000;Y:integer=-1000); reintroduce; overload;
    constructor CreateTOPRIGHT(MSG:String='');
    destructor DESTROY ; override;
//    procedure SetStep(Step:integer);
    procedure SetPos(Top,Left:integer);

    procedure ShowProgress;
    procedure HideProgress;
    property ProgressBarPos:integer read GetProgressbarPosition write SetProgressBarPosition;
    property ProgressBar:TProgressBar read GetProgress;
    procedure HIDE;
    procedure SHOW;

    property DopText:string read GetDopText write SetDopText;
  end;

implementation

uses bvWaitunitF2,bvUtils{,IniUnit};
constructor TMyWait.CreateTOPRIGHT(MSG:string='');
begin
  Create(MSG,-2000,-2000);
end;


constructor TMyWait.Create(MSG:string='';X:integer=-1000;Y:integer=-1000);
var i:integer;
    thTop,MaxTop:integer;
    thLeft:integer;
begin
  WaITC:=TWaitCursor.Create;
//  OutStepNN:=1;
  {if showNewForm
  then}
  begin
    thWaitForm:=twaitform3.Create(Application);
    if MSG='' then MSG:=StrIsWorkPleaseWait;
    (ThWaitForm as TWaitForm3).LabText.Caption:=MSG;
  end;
  //else thWaitForm:=tWaitform1.CreateMSG(Application,MSG);
//  Randomize;

//  thWaitForm.position:=poDesigned;
  if ((X<>-1000) or (Y<>-1000))
     and
     ((X<>-2000) or (Y<>-2000))
  then begin
    thWaitForm.Position:=poDesigned;
    thWaitForm.Top:=X;
    thWaitForm.Left:=y;
  end
  else begin
    if IndicatorPosOnEdge or ((X=-2000) and (Y=-2000))
    then begin
      thWaitForm.Position:=poDesigned;
      thTop:=10;
      thLeft:=Screen.width-thwaitform.Width-10;
      thWaitForm.Top:=thTop;
      thWaitForm.Left:=thLeft;
    end
    else begin
      thTop:=thWaitForm.Top;
      thLeft:=(Screen.width - thWaitForm.width) div 2;
    end;

    MaxTop:=Screen.Height-thWaitForm.Height;

    //  thWaitForm.AnimatedImage.GlyphNum:=random(thWaitForm.AnimatedImage.NumGlyphs);
    for i:=0 to Screen.FormCount-1 do
       if not (Screen.Forms[i]=thWaitForm) and ((Screen.Forms[i] is TWaitForm3) {or (Screen.Forms[i] is TWaitForm1)}  {or (Screen.Forms[i] is TWaitForm)})
//          and (Screen.Forms[i].Position<>poDesigned)
       then begin
         thTop:=Max(Screen.Forms[i].Top+Screen.Forms[i].Height,thTop);

         if thtop>maxtop then thtop:=maxtop;

         {$ifndef LINUX}
         (thWaitForm as  TwaitForm3).AnimateGlobe.visible:=false;
         (thWaitForm as  TwaitForm3).AnimateGlobe.Active:=false;
         {$endif}
         thWaitForm.position:=poDesigned;
         thWaitForm.Top:=thTop;
         thWaitForm.Left:=thLeft;

         if thTop>=MaxTop then begin
  //         thTop:=MaxTop;
           exit;
         end;
       end;

    //thWaitForm.Update;
  end;

  SHOW;
//  Priority:=tpTimeCritical;
  Inherited Create(Application);
end;

{
procedure TMyWait.SetStep(Step:integer);
begin
  OutStepNN:=Step;
  OutNNCount:=1;
end;
}
procedure TMyWait.SetPos(Top,Left:integer);
begin
  Hide;
  ThWaitForm.Top:=Top;
  ThWaitForm.Left:=Left;
  Show;
end;

procedure TMyWait.SetDopText(Text:string);
begin
{  if OutNNCount>=OutStepNN then begin}
    {if ThWaitForm is TWaitForm1 then begin
      (thWaitForm as TWaitForm1).LabNN.Caption:=Text;
      (thWaitForm as TWaitForm1).LabNN.Update;
    end
    else}
    begin
      //(thWaitForm as twaitform3).Caption:=Text;
      (ThWaitForm as TWaitForm3).LabText.Caption:=Text;

      (thWaitForm as twaitform3).update;
//      (thWaitForm as twaitform3).LabNN.Update;
    end;
{    OutNNCount:=1;
  end
  else inc(OutNNCount);}

  thWaitForm.Update
end;

function TMyWait.GetDopText:string;
begin
    {if ThWaitForm is TWaitForm1 then begin
      REsult:=(thWaitForm as TWaitForm1).LabNN.Caption;
    end
    else}
    begin
      Result:=  (ThWaitForm as TWaitForm3).LabText.Caption ; // (thWaitForm as twaitform3).Caption;
    end;
end;

destructor TMyWait.DESTROY;
begin
//  Suspend;
//  Terminate;
//  WaitFor;

  HIDE;

  //if ThWaitForm.visible then  thWaitForm.Close;

  thWaitForm.Free;
  WaitC.Free;
  //Application.Processmessages;

  inherited Destroy;
end;

function TMyWait.GetProgressbarPosition:integer;
begin
  if assigned(ThWaitForm) then begin
    {if (thWaitForm is TWaitForm1)
    then Result:=(thWaitForm as TWaitForm1).ProgressBar.Position
    else}
    Result:=(thWaitForm as twaitform3).ProgressBar.Position
  end
  else Result:=0;
end;

procedure TMyWait.SetProgressBarPosition(Value:integer);
begin
  if assigned(ThWaitForm) then begin
     {if ThWaitForm is TWaitForm1
     then (thWaitForm as TWaitForm1).Progressbar.Position:=Value
     else }
     if (Value<>0) and not (progressbar.Visible)
     then ShowProgress;
     
     (thWaitForm as twaitform3).ProgressBar.Position:=Value;
     thWaitForm.update
  end
end;

procedure TMyWait.ShowProgress;
begin
   if Assigned(THWaitForm) then begin
      {if ThWaitForm is tWaitForm1
      then  (thWaitForm as TWaitForm1).ShowProgress
      else}
      (thWaitForm as twaitform3).ShowProgress
   end;
end;

procedure TMyWait.HideProgress;
begin
   if Assigned(THWaitForm) then begin
      {if ThWaitForm is tWaitForm1
      then  (thWaitForm as TWaitForm1).HideProgress
      else  }
      (thWaitForm as twaitform3).HideProgress;
      thwaitForm.update;
   end;
end;

function TMyWait.GetProgress:TProgressBar;
begin
   if Assigned(THWaitForm) then begin
      {if ThWaitForm is tWaitForm1
      then  Result:=(thWaitForm as TWaitForm1).ProgressBar
      else  }
      Result:=(thWaitForm as twaitform3).ProgressBar;
   end
   else Result:=nil;
end;

procedure TMyWait.HIDE;
begin
  thWaitForm.Hide;
  updatescreen;
end;

procedure tMyWait.SHOW;
begin
  ThWaitForm.Show;
  //ThWaitForm.Update;
  UpdateScreen;
  {if assigned(application) and assigned(application.mainform)
     and application.mainform.visible
  then application.mainform.update;}
end;


end.
