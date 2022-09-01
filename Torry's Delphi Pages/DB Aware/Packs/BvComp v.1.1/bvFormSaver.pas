unit bvFormSaver;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,FileCtrl,
{$else}
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
{$endif}

  SysUtils,
  Classes,
  IniFiles,
  bvLocalization;


{$ifdef LINUX}
const
  CXHSCROLL = 12;
  CYHSCROLL = 12;
  CXVSCROLL = 18;
{$endif}

const
   psTop='Top';
   psLeft='Left';
   psWidth='Width';
   psHeight='Height';

   commonsect='common';

type
   TbvIniFile = class(TIniFile)
   public
     constructor Create(Filename:String); 
   end;


type
  TbvFormSaver = class(TComponent)
  private
    { Private declarations }
    FEnabled:boolean;
    FSavePosOnly:boolean;

    FOldOnFormDestroy:TNotifyEvent;

    function GetThisForm:TForm;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Loaded; override;

    constructor Create(AOwner:TComponent); override;
    procedure SaveFormPos;
    procedure SaveFormCoord;
    procedure RestoreFormPos;
    procedure RestoreFormCoord;
//    procedure BeforeDestruction; override;

    property ThisForm:TForm read GetThisForm;
    procedure OnDestroyForm(Sender:TObject);

    destructor destroy; override;

    procedure Restore;
    procedure Save;
    function GetIniFileName:string;

  published
    { Published declarations }
    property Enabled:boolean read FEnabled write FEnabled;
    property SavePosOnly:boolean read FSavePosOnly write FSavePosOnly;
  end;


procedure SaveForm(Form:TForm; PosOnly:boolean=false);
procedure RestoreForm(Form:TForm; PosOnly:boolean=false);

type FuncClientSize=function:integer of object;

var GetMainClientHeight:FuncClientSize;
    GetMainClientWidth:FuncClientSize;



implementation


function TbvFormSaver.GetIniFileName:string;
begin

  Result:=
     {$ifndef VER140}
        includetrailingbackslash
     {$else}
        includetrailingpathdelimiter
     {$endif}
     (
     {$ifndef VER140}
        includetrailingbackslash
     {$else}
        includetrailingpathdelimiter
     {$endif}

     (extractfilepath(Application.Exename))
     +'Forms'
     );

  if assigned(thisForm) then Result:=Result+ThisForm.name
  else Result:=Result+'NULLForm';

end;


constructor TbvFormSaver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FEnabled:=true;
  FSavePosOnly:=false;
  FOldOnFormDestroy:=nil;
//  ThisForm.Position:=podesigned;
//        if assigned(thisForm) and (Screen.Width<ThisForm.Width)
//        then thisform.Width:=Screen.width;

end;

procedure TbvFormSaver.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then begin

    if not assigned(FOldOnFormDestroy) then begin
      if (not ( csDesigning in ComponentState))
         and Assigned(ThisForm) {and (Self.Enabled)} then begin
        FOldOnFormDestroy:=ThisForm.OnDestroy;
        ThisForm.OnDestroy:=OnDestroyForm;
      end;

      Application.ProcessMessages;

      if Enabled then begin
        restore;
      end;

    end;
  end

end;

procedure TbvFormSaver.SaveFormPos;
begin
  if not (csDesigning in ComponentState) then begin
    if Enabled and assigned(ThisForm) then  With thisForm do begin
     with TbvIniFile.create(GetIniFileName) do
     try
       if not ((WindowState=wsMinimized))
       then begin
         WriteInteger(commonsect,psTop,Top);
         WriteInteger(commonsect,psLeft,Left);
       end
     finally
       free
     end;
    end
  end
end;

{
procedure TbvFormSaver.beforedestruction;
begin
  if not (csDesigning in ComponentState) then begin
    if Enabled then begin
      if SavePosOnly then SaveFormPos
      else SaveFormCoord;
    end;
    Enabled:=false;
  end
end;
}

procedure TbvFormSaver.OnDestroyForm(Sender:TObject);
var ThEvent:TNotifyEvent;
begin
  if not (csDesigning in ComponentState) then begin
    if Enabled then begin
      save;
    end;
    Enabled:=false;


    if assigned(FOldOnFormDestroy) then begin
      ThEvent:=FOldOnFormDestroy;
      FOldOnFormDestroy:=nil;
      ThEvent(sender);
    end;

  end
end;

procedure TbvFormSaver.SaveFormCoord;
begin
  if not (csDesigning in ComponentState) then
    if Enabled and assigned(ThisForm) then With thisForm do
    begin
     SaveFormPos;
     if not ({(WindowState=wsMaximized) or }(WindowState=wsMinimized))
     then
     with TbvIniFile.create(GetIniFileName) do
     try
       WriteInteger(commonsect,psWidth,Width);
       WriteInteger(commonsect,psHeight,Height);
     finally
       free
     end;
    end;
end;

procedure TbvFormSaver.RestoreFormPos;
var ThTop,ThLeft:integer;
begin

  if not (csDesigning in ComponentState) then
  with ThisForm do begin
//    if Twindowstate(thIniFile.ReadInteger(className,'State',ord(WindowState)))=wsMaximized
//    then begin
//      Windowstate:=wsMaximized
//       Top:=0;
//       Left:=0;
//    end
    {else}
    begin
        if FormStyle=fsMDIChild then begin
          ThTop:=0; ThLeft:=0;
        end
        else begin
          ThTop:=Top; ThLeft:=Left;
        end;

        if Enabled then
        with TbvIniFile.Create(GetINiFileName) do
        try
           Top:=ReadInteger(commonsect,psTop,ThTop);
           if Top<0 then Top:=0;
           Left:=ReadInteger(commonsect,psLeft,ThLeft);
           if Left<0 then Left:=0;
        finally
           free
        end;
    end
  end;

end;

procedure TbvFormSaver.RestoreFormCoord;
var MaxWidth,MaxHeight:integer;
    thHeight,thWidth:integer;
//    thState:TWindowState;
begin

  if not (csDesigning in ComponentState) then
  if enabled then begin
    if ThisForm.FormStyle=fsmdiChild then begin
       if Assigned(Application.MainForm) then begin
          if Assigned(Application.MainForm.HorzScrollbar)
          then  Application.MainForm.HorzScrollBar.Range:=0;
          if Assigned(application.MainForm.VertScrollBar)
          then  Application.MainForm.VertScrollBar.Range:=0;
       end;
    end;

    RestoreFormPos;
    with ThisForm do begin
       if FormStyle=fsMDIChild then begin
         with TbvIniFile.create(getInifileName) do
         try
           thHeight:=ReadInteger(commonsect,psHeight,0);
           thWidth:=ReadInteger(commonsect,psWidth,0);
         finally
           free
         end;
         if Assigned(Application.MainForm) then begin
           if Assigned(GetmainClientHeight) then begin
             MaxHeight:=GetMainClientHeight-
                {$ifndef  LINUX}
                Windows.GetSystemMetrics(SM_CYHSCROLL);
                {$else}
                   CYHSCROLL;
                {$endif}
           end
           else begin
             MaxHeight:=Application.MainForm.ClientHeight-ThisForm.Top-
                {$ifndef  LINUX}
                Windows.GetSystemMetrics(SM_CYHSCROLL);
                {$else}
                   CYHSCROLL;
                {$endif}
           end;
           if Assigned(GetmainClientWidth) then begin
             MaxWidth:=GetMainClientWidth-
                {$ifndef  LINUX}
                Windows.GetSystemMetrics(SM_CXVSCROLL);
                {$else}
                   CXVSCROLL;
                {$endif}
           end
           else begin
             MaxWidth:=Application.MainForm.ClientWidth-ThisForm.Left-
                {$ifndef  LINUX}
                Windows.GetSystemMetrics(SM_CXVSCROLL);
                {$else}
                   CXVSCROLL;
                {$endif}
           end;
           //MaxWidth:=Application.MainForm.ClientWidth-ThisForm.Left-Windows.GetSystemMetrics(SM_CXVSCROLL);
         end
         else begin
            MaxHeight:=480;
            MaxWidth:=640;
         end;

         if thHeight=0 then thHeight:=MaxHeight;
         if thWidth=0 then thWidth:=MaxWidth;

         if thHeight<0 then Height:=MaxHeight
         else Height:=thHeight;
         if ThWidth<0 then Width:=MaxWidth
         else Width:=THWidth;
       end
       else begin
         with TbvIniFile.Create(GetIniFileName) do
         try
           Height:=ReadInteger(commonsect,psHeight,Height);
           Width:=ReadInteger(commonsect,psWidth,Width);
         finally
           free
         end;
         MaxWidth:=Screen.Width-ThisForm.Left;
         MaxHeight:=Screen.Height-ThisForm.Top;
       end
    end;
    with ThisForm do begin
      if Width>MaxWidth then Width:=MaxWidth;
      if Height>MaxHeight then Height:=MaxHeight;
    end
  end;

end;

function TbvFormSaver.GetThisForm;
begin
  If Owner is TForm then Result:=(Owner as TForm)
  else Result:=nil;
end;



destructor TbvFormSaver.destroy;
begin
  inherited destroy;
end;

procedure TbvFormSaver.restore;
begin
  if saveposonly then restoreformpos
  else restoreformcoord;
end;

procedure TbvFormSaver.save;
begin
  if saveposonly then saveformpos
  else saveformcoord;
end;

procedure SaveForm(Form:TForm; PosOnly:boolean=false);
begin
  with TbvFormSaver.create(form) do
  try
    SavePosOnly:=PosOnly;
    Save;
  finally
    free
  end;
end;


procedure RestoreForm(Form:TForm; PosOnly:boolean=false);
begin
  with TbvFormSaver.create(form) do
  try
    SavePosOnly:=PosOnly;
    Restore;
  finally
    free
  end;
end;

constructor TBVIniFile.Create(Filename:String);
var Dir:string;
begin
  try
    Dir:=ExtractFilePath(Filename);
    if not DirectoryExists(Dir)
    then CreateDir(Dir);
  finally
    inherited  Create(Filename);
  end;
end;

end.
