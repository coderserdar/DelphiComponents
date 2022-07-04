unit API_label;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.03, 12072006, ari pikivirta
//  * added settings function and procedure
//
// r1.02, 06022006, ari pikivirta
//  * added linkedto property to allow label to follow some other component if moved
//  * fixed shadow drawing on state change in desinging state
//
// r1.01, 12082004, ari pikivirta
//  * changed shadowfont property name to fontshadow to get it close to
//    normal font property for easy changing of both

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Messages, Graphics, Types,
  API_base;

type
  TAPI_label = class(TAPI_Custom_Label)
  private
    fshadow: boolean;
    fshadowfont: tfont;
    fshadowoffset: integer;
    fcolormouse: tcolor;
    foldcolor: tcolor;
    fonmouseleave: tnotifyevent;
    fonmouseenter: tnotifyevent;
    furl: Ansistring;
    fcontrol: twincontrol;
    fobjectinstance: pointer;
    foldwindowproc: pointer;
    fposleft, fpostop: integer;
    procedure setshadowfont(f: tfont);
    procedure ControlWindowProc(var msg: TMessage);
    procedure SetControl(AValue : TWinControl);
    procedure setshadow(b: boolean);

  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Paint; override;
    procedure Click; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure MoveToPos; virtual;
    procedure TextOutAngle(C: TCanvas; X,Y: Integer; Angle: Word; Text: string);

  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

    procedure Settings(s: string); overload;
    function Settings: string; overload;

  published
    property LinkedTo: twincontrol read fcontrol write setcontrol;
    property ColorMouse: tcolor read fcolormouse write fcolormouse;
    property Shadow: boolean read fshadow write setshadow;
    property ShadowOffset: integer read fshadowoffset write fshadowoffset;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property GotoUrl: Ansistring read furl write furl;
    property Font;
    property FontShadow: tfont read fshadowfont write setshadowfont;

  end;

procedure Register;

implementation

{$WARN UNSAFE_CODE OFF}
{$R *.RES}

const
  versioninfostring = 'r1.03/ari.pikivirta@kolumbus.fi';

//------------------------------------------------------------------------------
constructor TAPI_label.create(aowner: tcomponent);
begin
  inherited create (aowner);
  transparent:=true;
  font.Color:=clblack;
  version:= versioninfostring;
  fshadowfont:= tfont.create;
  fshadowfont.assign (font);
  fshadowfont.Color:= clgray;
  fcolormouse:= clblue;
  fshadow:= true;
  foldcolor:= font.color;
  fshadowoffset:= -2;
  furl:='';
  FObjectInstance:= MakeObjectInstance(ControlWindowProc);
  fpostop:= 0;
  fposleft:= 0;
end;

//------------------------------------------------------------------------------
destructor TAPI_label.destroy;
begin
  fshadowfont.free;
  if Assigned(FControl) then
     SetWindowLong (TWinControl(FControl).Handle, GWL_WNDPROC, integer(FOldWindowProc));
  FreeObjectInstance(FObjectInstance);
  FObjectInstance := nil;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_label.Settings(s: string);
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    sl.Text:= s;
    colormouse:= strtoint(sl.Values['colormouse']);
    shadow:= strtobool(sl.values['shadow']);
    shadowoffset:= strtoint(sl.values['shadowoffset']);
    font.Name:= sl.values['font_name'];
    font.Size:= strtoint(sl.values['font_size']);
    font.color:= strtoint(sl.values['font_color']);
    font.style:= tfontstyles(tfontstyle(strtoint(sl.values['font_style'])));
    fontshadow.Name:= sl.values['fontshadow_name'];
    fontshadow.Size:= strtoint(sl.values['fontshadow_size']);
    fontshadow.color:= strtoint(sl.values['fontshadow_color']);
    fontshadow.style:= tfontstyles(tfontstyle(strtoint(sl.values['fontshadow_style'])));
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_label.Settings: string;
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    sl.clear;
    sl.add('colormouse='+inttostr(colormouse));
    sl.add('shadow='+booltostr(shadow));
    sl.add('shadowoffset='+inttostr(shadowoffset));
    sl.add('font_name='+font.Name);
    sl.add('font_size='+inttostr(font.size));
    sl.add('font_color='+inttostr(font.Color));
    sl.add('font_style='+inttostr(ord(tfontstyle(font.style))));
    sl.add('fontshadow_name='+fontshadow.name);
    sl.add('fontshadow_size='+inttostr(fontshadow.size));
    sl.add('fontshadow_color='+inttostr(fontshadow.Color));
    sl.add('fontshadow_style='+inttostr(ord(tfontstyle(fontshadow.style))));
    result:= sl.text;
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_label.setshadowfont(f: tfont);
begin
  if f<>fshadowfont then
  begin
    fshadowfont.Assign(f);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_label.MouseEnter(var Message: TMessage);
begin
  if (not (csdesigning in componentstate)) and (font.color<>fcolormouse) then
  begin
    foldcolor:=font.color;
    font.color:=fcolormouse;
    invalidate;
  end;
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_label.setshadow(b: boolean);
begin
  if b<>fshadow then
  begin
    fshadow:= b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_label.MouseLeave(var Message: TMessage);
begin
  if (not (csdesigning in componentstate)) and (font.color<>foldcolor) then
  begin
    font.color:=foldcolor;
    invalidate;
  end;
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_label.Click;
begin
  if furl<>'' then winexec(pansichar(furl), SW_SHOW);
  inherited click;
end;

//------------------------------------------------------------------------------
procedure TAPI_label.TextOutAngle(C: TCanvas; X,Y: Integer; Angle: Word; Text: string);
var
  LogRec: TLOGFONT;
  OldFontHandle, NewFontHandle: HFONT;
begin
  GetObject(c.Font.Handle, SizeOf(LogRec), Addr(LogRec));
  LogRec.lfEscapement := Angle*10;    //angle*10
  NewFontHandle := CreateFontIndirect(LogRec);
  OldFontHandle := SelectObject(c.Handle,NewFontHandle);
  c.Brush.Style := bsClear;
  c.TextOut(x,y,Text);
  NewFontHandle := SelectObject(c.Handle,OldFontHandle);
  DeleteObject(NewFontHandle);
end;

//------------------------------------------------------------------------------
procedure TAPI_label.paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  temprect: trect;
  len: integer;
begin
  len:= length(caption);

  TempRect := Rect(
    ClientRect.Left - fshadowoffset,
    ClientRect.Top - fshadowoffset,
    ClientRect.Right - fshadowoffset,
    ClientRect.Bottom - fshadowoffset);

  // background color
  canvas.Brush.Color:=color;
  if transparent then canvas.Brush.Style:=bsclear
    else canvas.Brush.Style:=bssolid;

  // shadow caption
  if fshadow then
  begin
    canvas.font.Assign (fshadowfont);
    DrawText(Canvas.Handle, pchar(caption), Len, TempRect,
      DT_EXPANDTABS or DT_WORDBREAK
      or Alignments[Alignment])
  end;

  // normal caption
  inherited paint;
end;

//------------------------------------------------------------------------------
procedure Tapi_label.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if ((Operation = opRemove)  and (AComponent = FControl)) then
    FControl := nil;
end;

//------------------------------------------------------------------------------
procedure TAPI_label.ControlWindowProc(var msg: TMessage);
begin
  if msg.Msg = WM_ENABLE then Enabled := FControl.Enabled;
  if msg.Msg = WM_MOVE then   MoveToPos;
  if msg.Msg = WM_SIZE then   MoveToPos;
  if msg.Msg = WM_DESTROY then
     SetWindowLong (TWinControl(FControl).Handle, GWL_WNDPROC, integer(FOldWindowProc));
  msg.result := CallWindowProc (fOldWindowProc, TWinControl(FControl).Handle, msg.msg, msg.wParam, msg.lParam)
end;

//------------------------------------------------------------------------------
procedure TAPI_label.SetControl(AValue: TWinControl);
begin
  if AValue <> FControl then
  begin
    if Assigned(FControl) then
        SetWindowLong (TWinControl(FControl).Handle, GWL_WNDPROC, integer(FOldWindowProc));
    FControl := AValue;
    if Assigned(FControl) then
    begin
      FControl.FreeNotification(Self);
      FOldWindowProc := TfnWndProc (SetWindowLong (TWinControl(FControl).Handle, GWL_WNDPROC, Integer (FObjectInstance)));
      // mark position difference
      fposleft:= left - fcontrol.left;
      fpostop:= top - fcontrol.top;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_label.Resize;
begin
  inherited;
  if Assigned(FControl) then
  begin
    fpostop:= top - fcontrol.top;
    fposleft:= left - fcontrol.left;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_label.MoveToPos;
begin
  if Assigned(FControl) then
    setbounds( fcontrol.left + fposleft, fcontrol.top + fpostop, width, height );
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_label]);
end;

end.
