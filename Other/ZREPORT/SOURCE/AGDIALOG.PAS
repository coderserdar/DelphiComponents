unit agDialog;

interface

uses Messages, Windows, SysUtils, Classes, Controls, Graphics, Forms,
     CommDlg, Dlgs, {DsgnIntf, }Consts, Printers, ShellAPI, Dialogs;

type

 TFilterString = string;

 { TFormCommonDialog }

  TFormCommonDialog = class(TGraphicControl)
  private
    FCtl3D: Boolean;
    FDefWndProc: Pointer;
    FHelpContext: THelpContext;
    FHandle: HWnd;
    FDialogHandle: HWnd;
    FObjectInstance: Pointer;
    FDialogTemplateHandle:THandle;
    FFormClientWidth,FFormClientHeight,
    FFormWidth,FFormHeight:integer;
    FDialogShowing:boolean;
    FForm:TForm;
    FOldFormParent:TWinControl;
    FOldFormParentHandle:HWND;
    FOldFormBorderStyle:TFormBorderStyle;
    FOldFormWndProc:TWndMethod;
    FUpdateFFormRect:boolean;
    FOldDialogWndProc:pointer;
    FSkipMessages:boolean;
    FTitle: string;
    FDesignBMPName:string;
    FDesignCaption:string;

    FOkButtonCaption:string;
    FCancelButtonCaption:string;
    FHelpButtonCaption:string;

    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnHelp:TNotifyEvent;

    function GetDialogTemplateHandle:THandle;virtual;abstract;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMInitDialog(var Message: TWMInitDialog); message WM_INITDIALOG;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMGETMINMAXINFO(var Message:TMessage); message WM_GETMINMAXINFO;
    procedure SetHeight(Value:integer);virtual;
    procedure SetWidth(Value:integer);virtual;
    procedure SetClientHeight(Value:integer);virtual;
    procedure SetClientWidth(Value:integer);virtual;
    procedure SetLeft(Value:integer);virtual;
    procedure SetTop(Value:integer);virtual;
    procedure SetForm(Form:TForm);

    function GetWidth:integer;
    function GetHeight:integer;
    function GetClientWidth:integer;
    function GetClientHeight:integer;
    function GetLeft:integer;
    function GetTop:integer;
  protected
    procedure SetOkButtonCaption(Value:string);
    procedure SetCancelButtonCaption(Value:string);
    procedure SetHelpButtonCaption(Value:string);
    procedure ValidateBounds;virtual;abstract;
    procedure Paint;override;
    function GetStaticRect: TRect; virtual;
    procedure DefaultHandler(var Message); override;
    procedure WndProc(var Message: TMessage); override;
    function MessageHook(var Msg: TMessage): Boolean; virtual;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): integer; virtual;
    procedure NewFormProc(var Message:TMessage);
    procedure DoClose; virtual;
    procedure DoShow; virtual;
    procedure DoHelp;virtual;
    function Execute: Boolean; virtual; abstract;

    property FormHeight:integer read GetHeight write SetHeight;
    property FormWidth:integer read GetWidth write SetWidth;
    property FormClientHeight:integer read GetClientHeight write SetClientHeight;
    property FormClientWidth:integer read GetClientWidth write SetClientWidth;
    property FormLeft:integer read GetLeft write SetLeft ;
    property FormTop:integer read GetTop write SetTop;
    property DialogTemplateHandle:THandle read GetDialogTemplateHandle;
    property Form:TForm read FForm write SetForm;
    property OldFormWndProc:TWndMethod read FOldFormWndProc;
    property OldDialogWndProc:pointer read FOldDialogWndProc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Handle: HWnd read FHandle;
    property DialogHandle: HWnd read FDialogHandle;
    property DialogShowing:boolean read FDialogShowing;

  published
    property OkButtonCaption:string read FOkButtonCaption write SetOkButtonCaption;
    property CancelButtonCaption:string read FCancelButtonCaption write SetCancelButtonCaption;
    property HelpButtonCaption:string read FHelpButtonCaption write SetHelpButtonCaption;
    property HelpContext:THelpContext read FHelpContext write FHelpContext default 0;
    property Ctl3D: Boolean read FCtl3D write FCtl3D default True;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property Title: string read FTitle write FTitle;

  end;


  { TFormPrintCommonDialog }

  TFormPrintCommonDialog = class(TFormCommonDialog)
  private
    FFromPage: Integer;
    FToPage: Integer;
    FCollate: Boolean;
    FOptions: TPrintDialogOptions;
    FPrintToFile: Boolean;
    FPrintRange: TPrintRange;
    FMinPage: Integer;
    FMaxPage: Integer;
    FCopies: Integer;
   protected
    procedure SetNumCopies(Value: Integer);
    procedure ValidateBounds;override;
    procedure DoShow;override;
  public
    function Execute: Boolean; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Collate: Boolean read FCollate write FCollate default False;
    property Copies: Integer read FCopies write SetNumCopies default 0;
    property FromPage: Integer read FFromPage write FFromPage default 0;
    property MinPage: Integer read FMinPage write FMinPage default 0;
    property MaxPage: Integer read FMaxPage write FMaxPage default 0;
    property Options: TPrintDialogOptions read FOptions write FOptions default [];
    property PrintToFile: Boolean read FPrintToFile write FPrintToFile default False;
    property PrintRange: TPrintRange read FPrintRange write FPrintRange default prAllPages;
    property ToPage: Integer read FToPage write FToPage default 0;
  end;

procedure Register;

implementation

procedure Register;
begin
  {
  RegisterComponents('ZReport', [TFormPrintCommonDialog]);
  }
  RegisterClasses([TFormPrintCommonDialog]);
  RegisterNoIcon ([TFormPrintCommonDialog]);
end;


type
  TCheatControl = class (TControl)
    procedure Cheat_UpdateBoundsRect(Control:TControl; R:TRect);
  end;

procedure TCheatControl.Cheat_UpdateBoundsRect(Control:TControl; R:TRect);
var OldSelf:TComponent;
begin
 OldSelf:=Self;
 Self:=TCheatControl(Control); //don't try this at home!
 Self.UpdateBoundsRect(R);
 Self:=TCheatControl(OldSelf);
end;


{ Private globals }

const
 FOpenDialogWidth:integer=410;
 FOpenDialogHeight:integer=280;
 FFontDialogWidth:integer=395;
 FFontDialogHeight:integer=319;
 FColorDialogWidth:integer=447;
 FColorDialogHeight:integer=299;
 FPrintDialogWidth:integer=432;
 FPrintDialogHeight:integer=302;
 FPrinterSetupDialogWidth:integer=432;
 FPrinterSetupDialogHeight:integer=289;
 FPageSetupDialogWidth:integer=360;
 FPageSetupDialogHeight:integer=390;
 FFolderDialogWidth: integer = 318;
 FFolderDialogHeight: integer = 306;

 FChangeIconDialogWidth:integer = 386;
 FChangeIconDialogHeight:integer = 249;
 FInsertObjectDialogWidth:integer = 440;
 FInsertObjectDialogHeight:integer = 244;


var
  CreationControl: TFormCommonDialog = nil;
  HelpMsg: Integer;
  WndProcPtrAtom: TAtom = 0;
  HookCtl3D: Boolean;

{ Center the given window on the screen }

procedure CenterWindow(Wnd: HWnd);
var
  Rect: TRect;
begin
  GetWindowRect(Wnd, Rect);
  SetWindowPos(Wnd, 0,
    (GetSystemMetrics(SM_CXSCREEN) - Rect.Right + Rect.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom + Rect.Top) div 3,
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;



{ Generic dialog hook. Centers the dialog on the screen in response to
  the WM_INITDIALOG message }

function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  Result := 0;
  case Msg of
    WM_INITDIALOG:
      begin
        if HookCtl3D then
        begin
          Subclass3DDlg(Wnd, CTL3D_ALL);
          SetAutoSubClass(True);
        end;
        CenterWindow(Wnd);
        CreationControl.FHandle := Wnd;
        CreationControl.FDialogHandle := GetParent(Wnd);
        CreationControl.FDefWndProc := Pointer(SetWindowLong(Wnd, GWL_WNDPROC,
          Longint(CreationControl.FObjectInstance)));
        CallWindowProc(CreationControl.FObjectInstance, Wnd, Msg, WParam, LParam);
        CreationControl := nil;
      end;
    WM_DESTROY:
     begin
      if HookCtl3D then SetAutoSubClass(False);
     end;
  end;
end;

{ TFormCommonDialog }

constructor TFormCommonDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCtl3D := True;
  FObjectInstance := MakeObjectInstance(WndProc);
  FDialogTemplateHandle:=0;
  Width:=100;
  Height:=100;
  Top:=0;
  Left:=0;
  if csDesigning in ComponentState then Visible:=true else Visible:=false;
  {Visible:=true;}
  FDialogShowing:=false;
  FHandle:=0;
  FDialogHandle:=0;
  FForm:=nil;
  FOldFormParent:=nil;
  FOldFormParentHandle:=0;
  FOldFormWndProc:=nil;
  FUpdateFFormRect:=false;
  FSkipMessages:=false;
  FDesignBMPName:='';
  OkButtonCaption:='&Ok';
  HelpButtonCaption:='&Help';
  CancelButtonCaption:='Cancel';
  FDesignCaption:='  At run time, Windows will'#13#10+
                  ' place controls in this area.';
end;

destructor TFormCommonDialog.Destroy;
begin
  if FObjectInstance <> nil then FreeObjectInstance(FObjectInstance);
  if FDialogTemplateHandle <> 0 then GlobalFree(FDialogTemplateHandle);
  inherited Destroy;
end;

procedure TFormCommonDialog.SetOkButtonCaption(Value:string);
begin
 if FHandle <> 0 then begin
  SendMessage(GetDlgItem(FDialogHandle,IDOK),WM_SETTEXT,0,integer(PChar(Value)));
 end;
 FOkButtonCaption:=Value;
end;

procedure TFormCommonDialog.SetCancelButtonCaption(Value:string);
begin
 if FHandle <> 0 then begin
  SendMessage(GetDlgItem(FDialogHandle,IDCANCEL),WM_SETTEXT,0,integer(PChar(Value)));
 end;
 FCancelButtonCaption:=Value;
end;

procedure TFormCommonDialog.SetHelpButtonCaption(Value:string);
begin
 if FHandle <> 0 then begin
  SendMessage(GetDlgItem(FDialogHandle,pshHelp),WM_SETTEXT,0,integer(PChar(Value)));
 end;
 FHelpButtonCaption:=Value;
end;

procedure TFormCommonDialog.Paint;
var R,R2:TRect;
    h:integer;
    BMP:TBitmap;
begin
 if csDesigning in ComponentState then begin
  ValidateBounds;
  BMP:=TBitmap.Create;
  try
   //this resource is present only at design time in the dsdialog.dcr file
   BMP.LoadFromResourceName(HInstance,FDesignBMPName);
  except
  end;                  
  if not BMP.Empty then
    Canvas.Draw((Self.Width-BMP.Width) div 2,
                (Self.Height-BMP.Height) div 2,BMP)
  else  //No bitmap resource :(
   with Canvas do begin
    R:=Rect(0,0,Width-1,Height-1);
    if Parent <> nil then begin
     Brush.Color:=clGray;
     FillRect(R);
    end;
    Pen.Color:=clWhite;
    MoveTo(0,Height-1);
    LineTo(0,0);
    LineTo(Width-1,0);
    Pen.Color:=clBlack;
    LineTo(Width-1,Height-1);
    LineTo(0,Height-1);
    Font.Name:='MS Sans Serif';
    Font.Size:=12;
    Font.Color:=clLime;
    R2:=R;
    h:=DrawText(Canvas.Handle, PChar(FDesignCaption), Length(FDesignCaption),
             R2, DT_CALCRECT or DT_LEFT or DT_NOPREFIX or DT_CENTER);
    R2:=Rect(R2.Left+80,R.Top+(((R2.Bottom-R2.Top)-h) div 2)+100,
             R2.Right+80,R.Bottom-(((R2.Bottom-R2.Top)-h) div 2)+100);
    DrawText(Canvas.Handle, PChar(FDesignCaption), Length(FDesignCaption),
             R2, DT_LEFT or DT_NOPREFIX or DT_CENTER);

   end;
   BMP.Destroy;
  end;
end;

function TFormCommonDialog.GetStaticRect: TRect;
begin
  if FHandle <> 0 then
  begin
    GetWindowRect(GetDlgItem(FHandle, stc32), Result);
    MapWindowPoints(0, FHandle, Result, 2);
  end
  else Result := Rect(0,0,0,0);
end;

procedure TFormCommonDialog.SetHeight(Value:integer);
var R:TREct;
begin
 if FDialogHandle <> 0 then begin
  GetWindowRect(FDialogHandle,R);
  SetWindowPos(FDialogHandle,0,0,0,R.Right-R.Left,Value,SWP_NOMOVE or SWP_NOZORDER);
  GetWindowRect(FDialogHandle,R);
  if FHandle <> FDialogHandle then begin
   Windows.GetClientRect(FDialogHandle,R);
   SetWindowPos(FHandle,0,0,0,R.Right-R.Left,R.Bottom-R.Top,SWP_NOMOVE or SWP_NOZORDER);
  end;
 end;
 FFormHeight:=Value;
 FFormClientHeight:=FormClientHeight;
end;

procedure TFormCommonDialog.SetWidth(Value:integer);
var R:TREct;
begin
 if FDialogHandle <> 0 then begin
  GetWindowRect(FDialogHandle,R);
  SetWindowPos(FDialogHandle,0,0,0,Value,R.Bottom-R.Top,SWP_NOMOVE or SWP_NOZORDER);
  if FHandle <> FDialogHandle then begin
   Windows.GetClientRect(FDialogHandle,R);
   SetWindowPos(FHandle,0,0,0,R.Right-R.Left,R.Bottom-R.Top,SWP_NOMOVE or SWP_NOZORDER);
  end;
 end;
 FFormWidth:=Value;
 FFormClientWidth:=FormClientWidth;
end;

procedure TFormCommonDialog.SetClientHeight(Value:integer);
var R1,R2:TREct;
begin
 if FDialogHandle <> 0 then begin
  Windows.GetWindowRect(FDialogHandle,R1);
  Windows.GetClientRect(FDialogHandle,R2);
  SetWindowPos(FDialogHandle,0,0,0,R1.Right-R1.Left,
               (R1.Bottom-R1.Top)+(Value-(R2.Bottom-R2.Top)),SWP_NOMOVE or SWP_NOZORDER);
  if FHandle <> FDialogHandle then begin
   Windows.GetClientRect(FDialogHandle,R1);
   SetWindowPos(FHandle,0,0,0,R1.Right-R1.Left,R1.Bottom-R1.Top,SWP_NOMOVE or SWP_NOZORDER);
  end;
 end;
 FFormClientHeight:=Value;
 FFormHeight:=FormHeight;
end;

procedure TFormCommonDialog.SetClientWidth(Value:integer);
var R1,R2:TREct;
begin
 if FDialogHandle <> 0 then begin
  Windows.GetWindowRect(FDialogHandle,R1);
  Windows.GetClientRect(FDialogHandle,R2);
  SetWindowPos(FDialogHandle,0,0,0,(R1.Right-R1.Left)+(Value-abs(R2.Right-R2.Left)),
               R1.Bottom-R1.Top,SWP_NOMOVE or SWP_NOZORDER);
  if FHandle <> FDialogHandle then begin
   Windows.GetClientRect(FDialogHandle,R1);
   SetWindowPos(FHandle,0,0,0,R1.Right-R1.Left,R1.Bottom-R1.Top,SWP_NOMOVE or SWP_NOZORDER);
  end;
 end;
 FFormClientWidth:=Value;
 FFormWidth:=FormWidth;
end;

procedure TFormCommonDialog.SetLeft(Value:integer);
 var R:TREct;
begin
 if FHandle <> 0 then begin
  GetWindowRect(FDialogHandle,R);
  MoveWindow(FDialogHandle,Value,R.Top,abs(R.Right-R.Left),abs(R.Top-R.Bottom),TRUE);
 end;
end;

procedure TFormCommonDialog.SetTop(Value:integer);
var R:TREct;
begin
 if FHandle <> 0 then begin
  GetWindowRect(FDialogHandle,R);
  MoveWindow(FDialogHandle,R.Left,Value,abs(R.Right-R.Left),abs(R.Top-R.Bottom),TRUE);
 end;
end;

procedure TFormCommonDialog.SetForm(Form:TForm);
begin
 FForm:=Form;
 {FForm.BorderStyle:=bsNone;}
end;

function TFormCommonDialog.GetWidth:integer;
var R:TRect;
begin
 if FHandle <> 0 then begin
  GetWindowRect(FDialogHandle,R);
  Result:=R.Right-R.Left;
 end
 else Result:=FFormWidth;
end;

function TFormCommonDialog.GetHeight:integer;
var R:TRect;
begin
 if FHandle <> 0 then begin
  GetWindowRect(FDialogHandle,R);
  Result:=R.Bottom-R.Top;
 end
 else Result:=FFormHeight;
end;

function TFormCommonDialog.GetClientWidth:integer;
var R:TRect;
begin
 if FHandle <> 0 then begin
  Windows.GetClientRect(FDialogHandle,R);
  Result:=R.Right-R.Left;
 end
 else Result:=FFormClientWidth;
end;

function TFormCommonDialog.GetClientHeight:integer;
var R:TRect;
begin
 if FHandle <> 0 then begin
  Windows.GetClientRect(FDialogHandle,R);
  Result:=R.Bottom-R.Top;
 end
 else Result:=FFormClientHeight;
end;

function TFormCommonDialog.GetLeft:integer;
var R:TRect;
begin
 if FHandle <> 0 then begin
  GetWindowRect(FDialogHandle,R);
  Result:=R.Left;
 end
 else Result:=0;
end;

function TFormCommonDialog.GetTop:integer;
var R:TRect;
begin
 if FHandle <> 0 then begin
  GetWindowRect(FDialogHandle,R);
  Result:=R.Top;
 end
 else Result:=0;
end;

function TFormCommonDialog.MessageHook(var Msg: TMessage): Boolean;
begin
  Result := False;
  if (Msg.Msg = HelpMsg) and (FHelpContext <> 0) then
  begin
    Application.HelpContext(FHelpContext);
    Result := True;
  end;
end;

procedure TFormCommonDialog.DefaultHandler(var Message);
begin
  if FHandle <> 0 then
    with TMessage(Message) do
      Result := CallWindowProc(FDefWndProc, FHandle, Msg, WParam, LParam)
  else inherited DefaultHandler(Message);
end;


procedure TFormCommonDialog.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_NOTIFY) then
    case (POFNotify(Message.LParam)^.hdr.code) of
      CDN_INITDONE: begin
       FormClientWidth:=FFormClientWidth;
       FormClientHeight:=FFormClientHeight;
       CenterWindow(FDialogHandle);
       DoShow;
      end;
      {WM_CTLCOLORDLG:
       begin
       Message.Result:=0;
       if FForm <> nil then begin
        Message.Result:=FForm.Canvas.Brush.Handle;
       end;
      end;}
    end;
  if (Message.Msg = WM_COMMAND) and (LongRec(Message.WParam).Lo = pshHelp) and
     (LongRec(Message.WParam).Hi = BN_CLICKED) then begin
      DoHelp;
     end;
  if (Message.Msg=WM_PAINT) then begin
    DefaultHandler(Message);
  end;
  if (Message.Msg=WM_WINDOWPOSCHANGING) and
     (csDesigning in ComponentState) then
   with TWindowPos(pointer(Message.lParam)^) do begin
    cx:=Self.Width;
    cy:=Self.Height;
   end;
  inherited WndProc(Message);
end;

procedure TFormCommonDialog.WMDestroy(var Message: TWMDestroy);
begin
  DoClose;
end;

procedure TFormCommonDialog.WMInitDialog(var Message: TWMInitDialog);
begin
  { Called only by non-explorer style dialogs }
  DoShow;
  { Prevent any further processing }
  Message.Result := 0;
end;

procedure TFormCommonDialog.WMNCDestroy(var Message: TWMNCDestroy);
begin
  inherited;
  FHandle := 0;
  FDialogHandle := 0;
end;

procedure TFormCommonDialog.WMGETMINMAXINFO(var Message:TMessage);
var S:TPoint;
begin
 with TMINMAXINFO(pointer(Message.lParam)^) do begin
  {S.X:=Self.Width;
  S.Y:=Self.Height;
  ptMaxSize:=S;
  ptMinTrackSize:=S;
  ptMaxTrackSize:=S;}
 end;
 Message.Result:=0;
end;

function TFormCommonDialog.TaskModalDialog(DialogFunc: Pointer; var DialogData): integer;
type
  TDialogFunc = function(var DialogData): Bool stdcall;
var
  ActiveWindow: HWnd;
  WindowList: Pointer;
begin
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
    Application.HookMainWindow(MessageHook);
    try
      CreationControl := Self;
      //prepare the slave form,
      //parent handle will be set in DoShow, since Common Dlg. is not shown yet
      if FForm <> nil then begin
       FForm.Visible:=false;
       FForm.Left:=-2000;        //move it off-screen
       FForm.Top:=-2000;
       if FForm.BorderStyle <> bsNone then begin
        FForm.Width:=FForm.ClientWidth;
        FForm.Height:=FForm.ClientHeight;
        FForm.BorderStyle:=bsNone;
       end;
       FForm.Visible:=true;     //make it visible off the screen
      end;
      Result := integer(TDialogFunc(DialogFunc)(DialogData));
    finally
      Application.UnhookMainWindow(MessageHook);
      //restore the slave form
      //parent handle was restored in DoClose
      if FForm <> nil then begin
       FForm.Visible:=false;
      end;
    end;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

const StrCommonDialogOldProc:PChar='DS.StrCommonDialogOldProc';

function NewDialogProc(Handle:HWND; Msg,wParam,lParam:longint):longint;stdcall;
var p:pointer;
    CF:TCheatControl;
    TP:TWINDOWPOS;
begin
 p:=Pointer(GetProp(Handle,StrCommonDialogOldProc)); //p points to TFormCommonDialog instance
 if p <> nil then begin
  case Msg of
   //Mirror changes in the common dialog position and
   //size to FForm
   {WM_WINDOWPOSCHANGING:
    begin
     with TFormCommonDialog(p).Form,TWindowPos(pointer(lParam)^) do
     if (Width<>cx) or (Height<>cy) then begin
      cx:=Width;
      cy:=Height;
    end;
   end; }
   {WM_CTLCOLORBTN, WM_CTLCOLOREDIT, WM_CTLCOLORLISTBOX,
   WM_CTLCOLORMSGBOX, WM_CTLCOLORSCROLLBAR, WM_CTLCOLORSTATIC:
     begin
      Result:=TFormCommonDialog(p).FForm.Canvas.Brush.Handle;
      Exit;
     end;}
   WM_WINDOWPOSCHANGED:
    begin
     CF:=TCheatControl.Create(nil);
     with TFormCommonDialog(p) do
           CF.Cheat_UpdateBoundsRect(FForm,Rect(FormLeft,FormTop,FormLeft+FormWidth,FormTop+FormHeight));
     CF.Destroy;
    end;
   WM_CLOSE:
    //Close the common dialog too
    begin
     TFormCommonDialog(p).Form.Visible:=false;
     Result:=0;
    end;
   end;
  Result:=CallWindowProc(TFormCommonDialog(p).OldDialogWndProc,Handle,Msg,wParam,lParam);
 end;
end;

//Overwrite some default FForm behavior
procedure TFormCommonDialog.NewFormProc(var Message:TMessage);
var w,h,l,t:integer;
    CF:TCheatControl;
    R:TRect;
begin
 with Message do begin
  case Msg of
  WM_WINDOWPOSCHANGING:
   if not FUpdateFFormRect then begin
    w:=TWindowPos(pointer(lParam)^).cx;
    h:=TWindowPos(pointer(lParam)^).cy;
    l:=TWindowPos(pointer(lParam)^).x;
    t:=TWindowPos(pointer(lParam)^).y;
    if FForm.WindowState <> wsNormal then begin
      FForm.WindowState:=wsNormal;
      GetWindowRect(FDialogHandle,R);
      TWindowPos(pointer(lParam)^).cx:=R.Right-R.Right;
      TWindowPos(pointer(lParam)^).cy:=R.Bottom-R.Top;
      TWindowPos(pointer(lParam)^).x:=R.Left;
      TWindowPos(pointer(lParam)^).y:=R.Top;
      Result:=1;
      Exit;
    end
    else begin
    //Adjust common dialog's size and position
    FormWidth:=w;
    FormHeight:=h;
    FormLeft:=l;
    FormTop:=t;
    FUpdateFFormRect:=true; //FForm's properties will be updated in WM_WINDOWPOSCHANGED
    //LockWindowUpdate(FDialogHandle);
    end;
   end;
  WM_WINDOWPOSCHANGED:
       //synchronize change in position
    if FUpdateFFormRect then begin
     FOldFormWndProc(Message);
     CF:=TCheatControl.Create(nil);
     FForm.SetBounds(0,0,FormClientWidth,FormClientHeight);
     CF.Cheat_UpdateBoundsRect(FForm,Rect(FormLeft,FormTop,FormLeft+FormWidth,FormTop+FormHeight));
     CF.Destroy;
     FUpdateFFormRect:=false;
     //LockWindowUpdate(0);
     Exit;
    end;
  WM_CLOSE:
    //Close the common dialog too
   begin
    Result:=SendMessage(FDialogHandle,WM_CLOSE,wParam,lParam);
   end;
  WM_SHOWWINDOW:
    //When hiding the window, close the common dialog too
   begin
    if wParam=0 then Result:=SendMessage(FDialogHandle,WM_CLOSE,wParam,lParam);
   end;
  WM_SETTEXT:
    //When FForms's caption changes, change the common dialog caption too
   begin
    FTitle:=PChar(lParam);
    Result:=SendMessage(FDialogHandle,WM_SETTEXT,wParam,lParam);
   end;
  CM_RECREATEWND:
    //Private VCL message: FForm needs to be recreated when some properties
    //(such as BorderStyle, BorderIcons, Control3D style, Position) change.
    //Don't let it do it!
   begin
    Result:=0;
    Exit;
   end;
  CM_VISIBLECHANGED:
   begin
    if (not FForm.Visible) and (not FSkipMessages) then begin
     FSkipMessages:=true;
     FForm.Visible:=true;
     FSkipMessages:=false;
     Result:=1;
     PostMessage(FDialogHandle,WM_CLOSE,wParam,lParam);
    end;
   end;
  WM_GETTEXT:
    //When Windows needs caption of the FForm, return Common Dialog caption
   begin
    Result:=SendMessage(FDialogHandle,WM_GETTEXT,wParam,lParam);
    Exit;
   end;
  WM_MOUSEMOVE,  WM_LBUTTONDOWN, WM_LBUTTONUP,
  WM_LBUTTONDBLCLK,WM_RBUTTONDOWN, WM_RBUTTONUP, WM_RBUTTONDBLCLK,
  WM_MBUTTONDOWN, WM_MBUTTONUP, WM_MBUTTONDBLCLK:
    //Some static controls do depent on these (Color Dlg for one)
    PostMessage(FDialogHandle,Msg,wParam,lParam);
  end;
  //call an old window proc
   FOldFormWndProc(Message);
 end;
end;


procedure TFormCommonDialog.DoClose;                               
begin
 if FForm <> nil then begin
   FForm.WindowProc:=FOldFormWndProc;
   FForm.Left:=-2000;
   FForm.Top:=-2000;
   FForm.Visible:=false;
   FForm.ParentWindow:=FOldFormParentHandle;
   FForm.Parent:=FOldFormParent;
   FOldFormParentHandle:=0;
   FOldFormParent:=nil;
   SetWindowLong(FDialogHandle,GWL_WNDPROC,Longint(FOldDialogWndProc));
   RemoveProp(FDialogHandle,StrCommonDialogOldProc);
   FOldFormWndProc:=nil;
   FForm.BorderStyle:=FOldFormBorderStyle;
   FForm.ClientWidth:=FFormClientWidth;
   FForm.ClientHeight:=FFormClientHeight;
   FForm.Realign;
  end;
  if Assigned(FOnClose) then FOnClose(Self);
  FDialogShowing:=false;
end;


procedure TFormCommonDialog.DoShow;
//var ic:THandle;
begin
  if FForm <> nil then begin
   FDialogShowing:=true;
   FormClientWidth:=FForm.ClientWidth;
   FormClientHeight:=FForm.ClientHeight;
   FOldFormBorderStyle:=FForm.BorderStyle;
   FForm.BorderStyle:=bsNone;
   FOldFormParent:=FForm.Parent;
   FOldFormParentHandle:=FForm.ParentWindow;
   FForm.Parent:=nil;
   FForm.ParentWindow:=FHandle;
   FForm.Left:=0;
   FForm.Top:=0;
   SetWindowPos(FForm.Handle, HWND_BOTTOM, 0, 0, 0, 0,
               SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW or SWP_NOACTIVATE);
   SendMessage(FDialogHandle,WM_SETTEXT,0,Integer(FForm.Caption)); //Set dialog's caption

   {ic:=SendMessage(FForm.Handle,WM_GETICON,0,0);
   SendMessage(FDialogHandle,WM_SETICON,0,ic);}
   //subclass the FForm window proc to trace some messages
   FOldFormWndProc:=FForm.WindowProc;
   FForm.WindowProc:=NewFormProc;
   //Subclass common dialog window proc
   FOldDialogWndProc:=pointer(SetWindowLong(FDialogHandle,GWL_WNDPROC,Longint(@NewDialogProc)));
   SetProp(FDialogHandle,StrCommonDialogOldProc,THandle(Self));
   //Fix a common dialog bug: hangs when windows tries to find the next tab-order control
   //Similar to q149501 (Microsoft Knowledge DB)
   SetWindowLong(Self.FHandle, GWL_EXSTYLE,GetWindowLong(Self.FHandle, GWL_EXSTYLE) or WS_EX_CONTROLPARENT);
   //Do the same with the FForm (in case the WS_EX_CONTROLPARENT bit was cleared)
   SetWindowLong(FForm.Handle, GWL_EXSTYLE,GetWindowLong(FForm.Handle, GWL_EXSTYLE) or WS_EX_CONTROLPARENT);
   OkButtonCaption:=OkButtonCaption;
   HelpButtonCaption:=HelpButtonCaption;
   CancelButtonCaption:=CancelButtonCaption;

  end;
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TFormCommonDialog.DoHelp;
begin
  if Assigned(FOnHelp) then FOnHelp(Self);
end;

{******************* TPrinterSetupDialog ***********************}

procedure GetPrinter(var DeviceMode, DeviceNames: THandle);
var
  Device, Driver, Port: array[0..79] of char;
  DevNames: PDevNames;
  Offset: PChar;
begin
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
  if DeviceMode <> 0 then
  begin
    DeviceNames := GlobalAlloc(GHND, SizeOf(TDevNames) +
     StrLen(Device) + StrLen(Driver) + StrLen(Port) + 3);
    DevNames := PDevNames(GlobalLock(DeviceNames));
    try
      Offset := PChar(DevNames) + SizeOf(TDevnames);
      with DevNames^ do
      begin
        wDriverOffset := Longint(Offset) - Longint(DevNames);
        Offset := StrECopy(Offset, Driver) + 1;
        wDeviceOffset := Longint(Offset) - Longint(DevNames);
        Offset := StrECopy(Offset, Device) + 1;
        wOutputOffset := Longint(Offset) - Longint(DevNames);;
        StrCopy(Offset, Port);
      end;
    finally
      GlobalUnlock(DeviceNames);
    end;
  end;
end;

procedure SetPrinter(DeviceMode, DeviceNames: THandle);
var
  DevNames: PDevNames;
begin
  DevNames := PDevNames(GlobalLock(DeviceNames));
  try
    with DevNames^ do
      Printer.SetPrinter(PChar(DevNames) + wDeviceOffset,
        PChar(DevNames) + wDriverOffset,
        PChar(DevNames) + wOutputOffset, DeviceMode);
  finally
    GlobalUnlock(DeviceNames);
    GlobalFree(DeviceNames);
  end;
end;

function CopyData(Handle: THandle): THandle;
var
  Src, Dest: PChar;
  Size: Integer;
begin
  if Handle <> 0 then
  begin
    Size := GlobalSize(Handle);
    Result := GlobalAlloc(GHND, Size);
    if Result <> 0 then
      try
        Src := GlobalLock(Handle);
        Dest := GlobalLock(Result);
        if (Src <> nil) and (Dest <> nil) then Move(Src^, Dest^, Size);
      finally
        GlobalUnlock(Handle);
        GlobalUnlock(Result);
      end
  end
  else Result := 0;
end;


{************* TFormPrintCommonDialog ************************}

procedure TFormPrintCommonDialog.ValidateBounds;
begin
 if (Width <> FPrintDialogWidth) then Width:=FPrintDialogWidth;
 if (Height <> FPrintDialogHeight) then Height:=FPrintDialogHeight;
 Left:=0;
 Top:=0;
end;

procedure TFormPrintCommonDialog.DoShow;
begin
 FDialogHandle:=FHandle;
 inherited;                                      
 CenterWindow(FDialogHandle);
end;
           
constructor TFormPrintCommonDialog.Create(AOwner: TComponent);
begin
  inherited;
  Self.Width:=FPrintDialogWidth;
  Self.Height:=FPrintDialogHeight;
  FDesignBMPName:='DS.TFormPrintCommonDialog.DESIGN.BITMAP';
end;

procedure TFormPrintCommonDialog.SetNumCopies(Value: Integer);
begin
  FCopies := Value;
  Printer.Copies := Value;
end;

function TFormPrintCommonDialog.Execute: Boolean;
const
  PrintRanges: array[TPrintRange] of Integer =
    (PD_ALLPAGES, PD_SELECTION, PD_PAGENUMS);
var
  PrintDlgRec: TPrintDlg;
  DevHandle: THandle;
  P:TWinControl;
begin
  FForm:=TForm(GetParentForm(Self));
  FillChar(PrintDlgRec, SizeOf(PrintDlgRec), 0);
  with PrintDlgRec do
  begin
    lStructSize := SizeOf(PrintDlgRec);
    hInstance := SysInit.HInstance;
    GetPrinter(DevHandle, hDevNames);
    hDevMode := CopyData(DevHandle);
    Flags := PrintRanges[FPrintRange] or (PD_ENABLEPRINTHOOK or
      PD_ENABLESETUPHOOK);                           
    if FCollate then Inc(Flags, PD_COLLATE);
    if not (poPrintToFile in FOptions) then Inc(Flags, PD_HIDEPRINTTOFILE);
    if not (poPageNums in FOptions) then Inc(Flags, PD_NOPAGENUMS);
    if not (poSelection in FOptions) then Inc(Flags, PD_NOSELECTION);
    if poDisablePrintToFile in FOptions then Inc(Flags, PD_DISABLEPRINTTOFILE);
    if FPrintToFile then Inc(Flags, PD_PRINTTOFILE);
    if poHelp in FOptions then Inc(Flags, PD_SHOWHELP);
    if not (poWarning in FOptions) then Inc(Flags, PD_NOWARNING);
    nFromPage := FFromPage;
    nToPage := FToPage;
    nMinPage := FMinPage;
    nMaxPage := FMaxPage;
    HookCtl3D := Ctl3D;
    lpfnPrintHook := DialogHook;
    lpfnSetupHook := DialogHook;
    hWndOwner := Application.Handle;
    Result := bool(TaskModalDialog(@PrintDlg, PrintDlgRec));
    if Result then
    begin
      SetPrinter(hDevMode, hDevNames);
      FCollate := Flags and PD_COLLATE <> 0;
      FPrintToFile := Flags and PD_PRINTTOFILE <> 0;
      if Flags and PD_SELECTION <> 0 then FPrintRange := prSelection else
        if Flags and PD_PAGENUMS <> 0 then FPrintRange := prPageNums else
          FPrintRange := prAllPages;
      FFromPage := nFromPage;
      FToPage := nToPage;
      if nCopies = 1 then
        Copies := Printer.Copies else
        Copies := nCopies;
    end
    else begin
      if hDevMode <> 0 then GlobalFree(hDevMode);
      if hDevNames <> 0 then GlobalFree(hDevNames);
    end;
  end;
end;

end.

