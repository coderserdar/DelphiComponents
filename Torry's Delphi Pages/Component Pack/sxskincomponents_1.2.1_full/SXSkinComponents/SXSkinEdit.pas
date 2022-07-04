unit SXSkinEdit;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses Windows, Classes, Controls, Messages, StdCtrls, SXSkinControl, Forms,
     SysUtils, Graphics, GR32, SXSkinLibrary, GR32_Blend, Types;

const

 VARE_W   = 1;
 VARE_H   = 2;

type

  {$IFNDEF COMPILER_9_UP}
  TSelection=record
   StartPos,EndPos:Integer;
  end;

  TWMPrint=packed record
   Msg:Cardinal;
   DC:HDC;
   Flags:Cardinal;
   Result:Integer;
  end;
  {$ENDIF}

  TSXSkinCustomEdit=class;

  TSXSkinCustomEditThread=class(TThread)
   public
    Control:TSXSkinCustomEdit;
    constructor Create;
    procedure Execute; override;
    procedure DoEvent;
  end;

  TSXSkinEditResetParam=(erpTextControl,
                         erpTextControlOnFontChange,
                         erpInvalidateOnStyleChange);

  TSXSkinEditResetParams=set of TSXSkinEditResetParam;

  TSXEditVariableComparer=class(TSXVariableComparer)
   private
    Control:TSXSkinCustomEdit;
    function GetValue(VarID:Integer):Integer;
   protected
    function VarListOnGetVariable(const VarName:String;var Error:Boolean):Single; override;
   public
    function GetVarValsForVarList(VarList:TList):TList; override;
    function Changed(VarList:TList;OldVarVals:TList):Boolean; override;
    procedure Update(VarList:TList;VarVals:TList); override;
    procedure DestroyVarList(VarList:TList); override;
    procedure DestroyVarVals(VarList:TList;VarVals:TList); override;
  end;

  TSXSkinCustomEditTextControl=class(TSXWinControl)
   private
    FParentEdit:TSXSkinCustomEdit;
    FOldSelLength:Integer;
    FOldSelStart:Integer;
    FCreating:Boolean;
    FModified:Boolean;
    FChanging:Boolean;
    FFontChanged:Boolean;
    FLastFocused:Boolean;
    LastClickTime:Cardinal;
    procedure CMEnter(var Message:TCMGotFocus); message CM_ENTER;
    procedure CNCommand(var Message:TWMCommand); message CN_COMMAND;
    procedure CMTextChanged(var Message:TMessage); message CM_TEXTCHANGED;
    procedure WMContextMenu(var Message:TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMEraseBkgnd(var Msg:TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Msg:TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg:TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseLeave(var Msg:TMessage); message WM_MOUSELEAVE;
    procedure WMMouseMove(var Msg:TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Msg:TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg:TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Msg:TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMFontChanged(var Message:TMessage); message CM_FONTCHANGED;
    procedure WMPrint(var Msg:TWMPrint); message WM_PRINT;
    procedure CNChar(var Msg:TMessage); message CN_CHAR;
    procedure CNKeyDown(var Msg:TMessage); message CN_KEYDOWN;
   protected
    function CapturesMouseAt(X,Y:Integer):Boolean; override;
    procedure Change; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoSetMaxLength(Value: Integer); virtual;
    procedure LocalKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    function LocalGetText:TCaption;
    procedure LocalSetText(Value:TCaption);
    procedure KeyDown(var Key:Word;Shift:TShiftState); override;
    procedure KeyUp(var Key:Word;Shift:TShiftState); override;
    procedure KeyPress(var Key:Char); override;
   public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; virtual;
    procedure DefaultHandler(var Message); override;
   published
    property TabStop default True;
    property Text:TCaption read LocalGetText write LocalSetText;
  end;

  TSXSkinCustomEdit=class(TSXSkinCustomControl)
   private
    FMaxLength:Integer;
    FPassword:Boolean;
    FReadOnly:Boolean;
    FAutoSelect:Boolean;
    FHideSelection:Boolean;
    FOEMConvert:Boolean;
    FCharCase:TEditCharCase;
    FOnChange:TNotifyEvent;
    FTextControl:TSXSkinCustomEditTextControl;
    FMouseOver:Boolean;
    FOnMouseEnter:TNotifyEvent;
    FOnMouseLeave:TNotifyEvent;
    FOnUserModified:TNotifyEvent;
    FAutoSizeHeight:Boolean;
    FFrequentChange:Boolean;
    FAlignment:TAlignment;
    //
    FLastEditTransform:TSXTransformEffectData;
    FDoneSteps:Integer;
    FLastEdit:TBitmap32;
    FThread:TSXSkinCustomEditThread;
    //
    FLastStyle:String;
    FLastFontData:TSXFontData;
    FLastChangedValue:String;
    FBackBrush:TBrush;
    FBrushBitmap:TBitmap;
    CEID_Back:Integer;
    VComparer:TSXEditVariableComparer;
    function GetModified:Boolean;
    function GetCanUndo:Boolean;
    function GetTabStop:Boolean;
    function GetTabOrder:Integer;
    function GetText:String;
    procedure SetText(const Value:String);
    procedure SetCharCase(Value:TEditCharCase);
    procedure SetHideSelection(Value:Boolean);
    procedure SetMaxLength(Value:Integer);
    procedure SetModified(Value:Boolean);
    procedure SetOEMConvert(Value:Boolean);
    procedure SetPassword(Value:Boolean);
    procedure SetReadOnly(Value:Boolean);
    procedure SetSelText(const Value:String);
    procedure SetTabStop(Value:Boolean);
    procedure SetTabOrder(Value:Integer);
    procedure SetAutoSizeHeight(Value:Boolean);
    procedure SetAlignment(Value:TAlignment);
    function OnGetVariable(const VarName:String;var Error:Boolean):Single;
    procedure CMFontChanged(var Message:TMessage); message CM_FONTCHANGED;
    procedure WMCtlColorEdit(var Msg:TMessage); message WM_CTLCOLOREDIT;
    procedure WMCtlColorStatic(var Msg:TMessage); message WM_CTLCOLORSTATIC;
    procedure InternalMouseEnter;
    procedure InternalMouseLeave;
    procedure InvalidateEdit;
    procedure DoThreadActions;
    procedure CreateThreadIfNeeded;
    function HasUnusualSkinStyle:Boolean;
    procedure GetCurrentTransformEffect(Action:TSXGlyphChangeAction;var Effect:TSXTransformEffectData);
   protected
    procedure MouseLeave; override;
    procedure MouseMove(Shift:TShiftState;X,Y:Integer); override;
    function CapturesMouseAt(X,Y:Integer):Boolean; override;
    procedure SetEnabled(Value:Boolean); override;
    procedure Loaded; override;
    procedure PaintCurrentEStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
    procedure PaintCurrentBlendedEStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
    procedure DoSetMaxLength(Value: Integer); virtual;
    function GetSelLength: Integer; virtual;
    function GetSelStart: Integer; virtual;
    function GetSelText: string; virtual;
    procedure SetSelLength(Value: Integer); virtual;
    procedure SetSelStart(Value: Integer); virtual;
    procedure GetCurrentEState(var EState:TSXSkinEditStateParam);
    procedure StartChanging(T:TSXGlyphChangeAction);
    procedure ResetEditParams(Params:TSXSkinEditResetParams=[]);
    procedure InternalSetBounds(ALeft,ATop,AWidth,AHeight:Integer);
    property AutoSelect:Boolean read FAutoSelect write FAutoSelect default True;
    property CharCase:TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property HideSelection:Boolean read FHideSelection write SetHideSelection default True;
    property OEMConvert:Boolean read FOEMConvert write SetOEMConvert default False;
    property Password:Boolean read FPassword write SetPassword default False;
    property ParentColor default False;
   public
    function IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean; override;
    procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
              Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
              WithSubItems:Boolean); override;
    procedure SkinChanged; override;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
    procedure Clear; virtual;
    procedure ClearSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure ClearUndo;
    function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer; virtual;
    procedure SelectAll;
    procedure SetSelTextBuf(Buffer: PChar);
    procedure InvalidateIfStyleChanged;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    property Alignment:TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property CanUndo:Boolean read GetCanUndo;
    property MaxLength:Integer read FMaxLength write SetMaxLength default 0;
    property Modified:Boolean read GetModified write SetModified;
    property ReadOnly:Boolean read FReadOnly write SetReadOnly default False;
    property SelLength:Integer read GetSelLength write SetSelLength;
    property SelStart:Integer read GetSelStart write SetSelStart;
    property SelText:String read GetSelText write SetSelText;
    property Text:String read GetText write SetText;
    property SkinStyle stored HasUnusualSkinStyle;
    property AutoSizeHeight:Boolean read FAutoSizeHeight write SetAutoSizeHeight default True;
    property FrequentChange:Boolean read FFrequentChange write FFrequentChange default False;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property OnUserModified:TNotifyEvent read FOnUserModified write FOnUserModified;    
   published
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnKeyDown;
    property OnKeyPress;
    property TabStop:Boolean read GetTabStop write SetTabStop default True;
    property TabOrder:Integer read GetTabOrder write SetTabOrder;
    property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  TSXSkinEdit=class(TSXSkinCustomEdit)
   published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSizeHeight;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FrequentChange;
    property HideSelection;
    //property HintData;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property Password;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property SkinLibrary;
    property SkinStyle;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUserModified;
  end;

implementation

uses SXBitmap32Utils;

{ TSXSkinCustomEditThread }

constructor TSXSkinCustomEditThread.Create;
begin
 inherited Create(True);
 FreeOnTerminate:=False;
end;

procedure TSXSkinCustomEditThread.Execute;
begin
 while not Terminated do
  begin
   SleepEx(30,True);
   if not Suspended then
    Synchronize(DoEvent);
  end;
end;

procedure TSXSkinCustomEditThread.DoEvent;
begin
 if Assigned(Control) then
  Control.DoThreadActions;
end;

{ TSXEditVariableComparer }

function TSXEditVariableComparer.VarListOnGetVariable(const VarName:String;var Error:Boolean):Single;
var CurVarVal:Integer;
begin
 Result:=1234;
 CurVarVal:=-1;
 if VarName='W' then
  CurVarVal:=VARE_W else
 if VarName='H' then
  CurVarVal:=VARE_H;
 if CurVarVal>=0 then
  begin
   if CurValList=nil then
    CurValList:=TList.Create;
   CurValList.Add(Pointer(CurVarVal));
  end;
end;

function TSXEditVariableComparer.GetValue(VarID:Integer):Integer;
begin
 Result:=0;
 if Control<>nil then
  begin
   case VarID of
    VARE_W: Result:=Control.Width;
    VARE_H: Result:=Control.Height;
   end;
  end;
end;

function TSXEditVariableComparer.GetVarValsForVarList(VarList:TList):TList;
var A:Integer;
begin
 if VarList=nil then
  begin
   Result:=nil;
   exit;
  end;
 Result:=TList.Create;
 for A:=0 to VarList.Count-1 do
  Result.Add(Pointer(GetValue(Integer(VarList[A]))));
end;

function TSXEditVariableComparer.Changed(VarList:TList;OldVarVals:TList):Boolean;
var A:Integer;
begin
 Result:=False;
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  if Integer(OldVarVals[A])<>GetValue(Integer(VarList[A])) then
   begin
    Result:=True;
    exit;
   end;
end;

procedure TSXEditVariableComparer.Update(VarList:TList;VarVals:TList);
var A:Integer;
begin
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  VarVals[A]:=Pointer(GetValue(Integer(VarList[A])));
end;

procedure TSXEditVariableComparer.DestroyVarList(VarList:TList);
begin
 VarList.Free;
end;

procedure TSXEditVariableComparer.DestroyVarVals(VarList:TList;VarVals:TList);
begin
 VarVals.Free;
end;

{ TSXSkinCustomEditTextControl }

constructor TSXSkinCustomEditTextControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 if AOwner is TSXSkinCustomEdit then
  FParentEdit:=TSXSkinCustomEdit(AOwner);
 ControlStyle:=[csClickEvents,csSetCaption,csDoubleClicks{$IFDEF COMPILER_10_UP},csPannable{$ENDIF}];
 TabStop:=True;
 ParentColor:=False;
 inherited OnKeyDown:=LocalKeyDown;
end;

function TSXSkinCustomEditTextControl.CapturesMouseAt(X,Y:Integer):Boolean;
begin
 Result:=False;
end;

procedure TSXSkinCustomEditTextControl.DoSetMaxLength(Value: Integer);
begin
 SendMessage(Handle,EM_LIMITTEXT,Value,0);
end;

procedure TSXSkinCustomEditTextControl.Clear;
begin
 SetWindowText(Handle,'');
end;

procedure TSXSkinCustomEditTextControl.CreateParams(var Params:TCreateParams);
const Passwords:array[Boolean]of DWORD=(0,ES_PASSWORD);
      ReadOnlys:array[Boolean]of DWORD=(0,ES_READONLY);
      CharCases:array[TEditCharCase]of DWORD=(0,ES_UPPERCASE,ES_LOWERCASE);
      HideSelections:array[Boolean]of DWORD=(ES_NOHIDESEL,0);
      OEMConverts:array[Boolean]of DWORD=(0,ES_OEMCONVERT);
      AlignmentStyles:array[TAlignment]of DWORD=(ES_LEFT,ES_RIGHT,ES_CENTER);
begin
 inherited CreateParams(Params);
 CreateSubClass(Params,'EDIT');
 with Params do
  begin
   Style:=Style or (ES_AUTOHSCROLL or ES_AUTOVSCROLL) or
      Passwords[FParentEdit.FPassword] or ReadOnlys[FParentEdit.FReadOnly] or
      CharCases[FParentEdit.FCharCase] or HideSelections[FParentEdit.FHideSelection] or
      OEMConverts[FParentEdit.FOEMConvert] or AlignmentStyles[FParentEdit.FAlignment];
  end;
end;

procedure TSXSkinCustomEditTextControl.CreateWindowHandle(const Params: TCreateParams);
var P:TCreateParams;
begin
 if SysLocale.FarEast and (Win32Platform<>VER_PLATFORM_WIN32_NT) and
    (Params.Style and ES_READONLY<>0) then
  begin
   P:=Params;
   P.Style:=P.Style and not ES_READONLY;
   inherited CreateWindowHandle(P);
   if WindowHandle<>0 then
    SendMessage(WindowHandle,EM_SETREADONLY,Ord(True),0);
  end else inherited CreateWindowHandle(Params);
end;

procedure TSXSkinCustomEditTextControl.CreateWnd;
begin
 FCreating:=True;
 try
  inherited CreateWnd;
 finally
  FCreating:=False;
 end;
 DoSetMaxLength(FParentEdit.FMaxLength);
 FParentEdit.Modified:=FModified;
 if FOldSelStart<>-1 then
  FParentEdit.SelStart:=FOldSelStart;
 if FOldSelLength<>-1 then
  FParentEdit.SelLength:=FOldSelLength;
end;

procedure TSXSkinCustomEditTextControl.DestroyWnd;
begin
 FModified:=FParentEdit.Modified;
 FOldSelLength:=FParentEdit.SelLength;
 FOldSelStart:=FParentEdit.SelStart;
 inherited DestroyWnd;
end;

procedure TSXSkinCustomEditTextControl.Change;
begin
 inherited;
 if FParentEdit<>nil then
  begin
   if Assigned(FParentEdit.FOnChange) then
    FParentEdit.FOnChange(Self);
   if not FChanging and Assigned(FParentEdit.FOnUserModified) and FParentEdit.FFrequentChange then
    begin
     if LocalGetText<>FParentEdit.FLastChangedValue then
      begin
       FParentEdit.FOnUserModified(FParentEdit);
       FParentEdit.FLastChangedValue:=LocalGetText;
      end;
    end;
  end;
end;

procedure TSXSkinCustomEditTextControl.DefaultHandler(var Message);
begin
  case TMessage(Message).Msg of
    WM_SETFOCUS:
      if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
        not IsWindow(TWMSetFocus(Message).FocusedWnd) then
        TWMSetFocus(Message).FocusedWnd := 0;
  end;
  inherited;
end;

procedure TSXSkinCustomEditTextControl.CNCommand(var Message:TWMCommand);
begin
 if (Message.NotifyCode=EN_CHANGE) and not FCreating then Change;
end;

procedure TSXSkinCustomEditTextControl.CMEnter(var Message: TCMGotFocus);
begin
 if FParentEdit.FAutoSelect and not (csLButtonDown in ControlState) and
    (GetWindowLong(Handle,GWL_STYLE) and ES_MULTILINE=0) then FParentEdit.SelectAll;
 inherited;
end;

procedure TSXSkinCustomEditTextControl.CMTextChanged(var Message:TMessage);
begin
 inherited;
 if not HandleAllocated or (GetWindowLong(Handle,GWL_STYLE) and ES_MULTILINE<>0) then Change;
end;

procedure TSXSkinCustomEditTextControl.WMContextMenu(var Message:TWMContextMenu);
begin
 SetFocus;
 inherited;
end;

procedure TSXSkinCustomEditTextControl.WMEraseBkgnd(var Msg:TWmEraseBkgnd);
begin
 Msg.Result:=1;
end;

procedure TSXSkinCustomEditTextControl.WMSetFocus(var Msg:TWMSetFocus);
begin
 if FParentEdit<>nil then
  begin
   FParentEdit.StartChanging(gcaFocus);
   FLastFocused:=True;
   FParentEdit.InvalidateIfStyleChanged;
  end;
 inherited;
end;

procedure TSXSkinCustomEditTextControl.WMKillFocus(var Msg:TWMKillFocus);
begin
// ShowCaret(Handle);
 if FParentEdit<>nil then
  begin
   FParentEdit.StartChanging(gcaUnfocus);
   FLastFocused:=False;
   FParentEdit.InvalidateIfStyleChanged;
   if Assigned(FParentEdit.FOnUserModified) then
    if LocalGetText<>FParentEdit.FLastChangedValue then
     begin
      FParentEdit.FOnUserModified(FParentEdit);
      FParentEdit.FLastChangedValue:=LocalGetText;
     end;
  end;
 inherited;
end;

procedure TSXSkinCustomEditTextControl.WMMouseLeave(var Msg:TMessage);
begin
 inherited;
 if FParentEdit<>nil then
  SendMessage(FParentEdit.Handle,WM_MOUSELEAVE,0,0);
end;

procedure TSXSkinCustomEditTextControl.WMMouseMove(var Msg:TWMMouseMove);
begin
 inherited;
 if FParentEdit<>nil then
  SendMessage(FParentEdit.Handle,WM_MOUSEMOVE,TMessage(Msg).WParam,
      (Msg.XPos+Left) or ((Msg.YPos+Top) shl 16));
end;

procedure TSXSkinCustomEditTextControl.WMLButtonDown(var Msg:TWMLButtonDown);
begin
 LastClickTime:=GetTickCount;
 inherited;
 if FParentEdit<>nil then
  SendMessage(FParentEdit.Handle,WM_LBUTTONDOWN,TMessage(Msg).WParam,
      (Msg.XPos+Left) or ((Msg.YPos+Top) shl 16));
end;

procedure TSXSkinCustomEditTextControl.WMLButtonUp(var Msg:TWMLButtonUp);
begin
 inherited;
 if FParentEdit<>nil then
  SendMessage(FParentEdit.Handle,WM_LBUTTONUP,TMessage(Msg).WParam,
      (Msg.XPos+Left) or ((Msg.YPos+Top) shl 16));
end;

procedure TSXSkinCustomEditTextControl.WMLButtonDblClk(var Msg:TWMLButtonDblClk);
begin
 inherited;
 if FParentEdit<>nil then
  SendMessage(FParentEdit.Handle,WM_LBUTTONDBLCLK,TMessage(Msg).WParam,
      (Msg.XPos+Left) or ((Msg.YPos+Top) shl 16));
end;

procedure TSXSkinCustomEditTextControl.CMFontChanged(var Message:TMessage);
begin
 inherited;
 FFontChanged:=True;
end;

procedure TSXSkinCustomEditTextControl.WMPrint(var Msg:TWMPrint);
var    P:TPoint;
   XX,YY:Integer;
 ACanvas:TCanvas;
   BTime:Cardinal;
begin
 inherited;
 if Focused then
  begin
   BTime:=GetCaretBlinkTime;
   if GetCaretPos(P) and ((GetTickCount-LastClickTime) mod (BTime*2)<BTime) and PaintCaret then
    begin
     ACanvas:=TCanvas.Create;
     try
      ACanvas.Handle:=Msg.DC;
      ACanvas.Pen.Style:=psSolid;
      ACanvas.Pen.Mode:=pmXor;
      ACanvas.Pen.Color:=clWhite;
      ACanvas.Brush.Style:=bsClear;
      XX:=P.X;
      YY:=P.Y;
      ACanvas.MoveTo(XX,YY);
      ACanvas.LineTo(XX,YY+Height);
     finally
      ACanvas.Free;
     end;
    end;
  end;
end;

procedure TSXSkinCustomEditTextControl.CNChar(var Msg:TMessage);
begin
 LastClickTime:=GetTickCount;
 inherited;
end;

procedure TSXSkinCustomEditTextControl.CNKeyDown(var Msg:TMessage);
begin
 LastClickTime:=GetTickCount;
 inherited;
end;

procedure TSXSkinCustomEditTextControl.LocalKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if (FParentEdit<>nil) and Assigned(FParentEdit.OnKeyDown) then
  FParentEdit.OnKeyDown(Sender,Key,Shift);
 if Key=VK_RETURN then
  if (FParentEdit<>nil) and Assigned(FParentEdit.FOnUserModified) then
   begin
    if LocalGetText<>FParentEdit.FLastChangedValue then
     begin
      FParentEdit.FOnUserModified(FParentEdit);
      FParentEdit.FLastChangedValue:=LocalGetText;
     end;
    Parent.SetFocus;
   end else
 if Key=VK_ESCAPE then
  begin
   if FParentEdit<>nil then
    Text:=FParentEdit.FLastChangedValue;
   Parent.SetFocus;
  end;
end;

function TSXSkinCustomEditTextControl.LocalGetText:TCaption;
var Len:Integer;
begin
 Len:=GetTextLen;
 SetString(Result,PChar(nil),Len);
 if Len<>0 then GetTextBuf(Pointer(Result),Len+1);
end;

procedure TSXSkinCustomEditTextControl.LocalSetText(Value:TCaption);
begin
 if LocalGetText<>Value then
  begin
   FChanging:=True;
   SetTextBuf(PChar(Value));
   FChanging:=False;
   if FParentEdit<>nil then
    FParentEdit.FLastChangedValue:=Value;
  end;
end;

procedure TSXSkinCustomEditTextControl.KeyDown(var Key:Word;Shift:TShiftState);
begin
 if Assigned(FParentEdit.OnKeyDown) then
  FParentEdit.OnKeyDown(FParentEdit,Key,Shift);
 inherited;
end;

procedure TSXSkinCustomEditTextControl.KeyUp(var Key:Word;Shift:TShiftState);
begin
 if Assigned(FParentEdit.OnKeyUp) then
  FParentEdit.OnKeyUp(FParentEdit,Key,Shift);
 inherited;
end;

procedure TSXSkinCustomEditTextControl.KeyPress(var Key:Char);
begin
 if Assigned(FParentEdit.OnKeyPress) then
  FParentEdit.OnKeyPress(FParentEdit,Key);
 inherited;
end;

{ TSXSkinCustomEdit }

procedure TSXSkinCustomEdit.SetEnabled(Value:Boolean);
begin
 if Enabled<>Value then
  begin
   if not (csLoading in ComponentState) then
    begin
     if Enabled then
      StartChanging(gcaDisable) else
       StartChanging(gcaEnable);
    end;
   inherited;
   if not Enabled then
    FMouseOver:=False;
   if not (csLoading in ComponentState) then
    ResetEditParams([erpTextControlOnFontChange,erpInvalidateOnStyleChange]);
  end;
end;

function TSXSkinCustomEdit.HasUnusualSkinStyle:Boolean;
begin
 Result:=SkinStyle<>'_Edit';
end;

procedure TSXSkinCustomEdit.DoSetMaxLength(Value: Integer);
begin
 SendMessage(FTextControl.Handle,EM_LIMITTEXT,Value,0);
end;

procedure TSXSkinCustomEdit.SetCharCase(Value: TEditCharCase);
begin
 if FCharCase<>Value then
  begin
   FCharCase:=Value;
   RecreateWnd;
  end;
end;

procedure TSXSkinCustomEdit.SetHideSelection(Value: Boolean);
begin
 if FHideSelection<>Value then
  begin
   FHideSelection:=Value;
   RecreateWnd;
  end;
end;

procedure TSXSkinCustomEdit.SetMaxLength(Value: Integer);
begin
 if FMaxLength<>Value then
  begin
   FMaxLength:=Value;
   if FTextControl.HandleAllocated then DoSetMaxLength(Value);
  end;
end;

procedure TSXSkinCustomEdit.SetOEMConvert(Value: Boolean);
begin
 if FOEMConvert<>Value then
  begin
   FOEMConvert:=Value;
   FTextControl.RecreateWnd;
  end;
end;

function TSXSkinCustomEdit.GetModified:Boolean;
begin
 Result:=FTextControl.FModified;
 if FTextControl.HandleAllocated then
  Result:=SendMessage(FTextControl.Handle,EM_GETMODIFY,0,0)<>0;
end;

function TSXSkinCustomEdit.GetCanUndo:Boolean;
begin
  Result:=False;
  if FTextControl.HandleAllocated then
   Result:=SendMessage(FTextControl.Handle,EM_CANUNDO,0,0)<>0;
end;

procedure TSXSkinCustomEdit.SetModified(Value:Boolean);
begin
 if FTextControl.HandleAllocated then
  SendMessage(FTextControl.Handle,EM_SETMODIFY,Byte(Value),0) else
   FTextControl.FModified:=Value;
end;

procedure TSXSkinCustomEdit.SetPassword(Value:Boolean);
begin
 if FPassword<>Value then
  begin
   FPassword:=Value;
   if FTextControl.HandleAllocated then
    FTextControl.RecreateWnd;
  end;
end;

procedure TSXSkinCustomEdit.SetReadOnly(Value:Boolean);
begin
 if FReadOnly<>Value then
  begin
   FReadOnly:=Value;
   if FTextControl.HandleAllocated then
    SendMessage(FTextControl.Handle,EM_SETREADONLY,Ord(Value),0);
  end;
end;

procedure TSXSkinCustomEdit.SetAlignment(Value:TAlignment);
begin
 if FAlignment<>Value then
  begin
   FAlignment:=Value;
   if FTextControl.HandleAllocated then
    FTextControl.RecreateWnd;
  end;
end;

function TSXSkinCustomEdit.GetSelStart:Integer;
begin
 SendMessage(FTextControl.Handle,EM_GETSEL,Longint(@Result),0);
end;

procedure TSXSkinCustomEdit.SetSelStart(Value:Integer);
begin
 SendMessage(FTextControl.Handle,EM_SETSEL,Value,Value);
end;

function TSXSkinCustomEdit.GetSelLength:Integer;
var Selection:TSelection;
begin
 SendMessage(FTextControl.Handle,EM_GETSEL,Longint(@Selection.StartPos),Longint(@Selection.EndPos));
 Result:=Selection.EndPos-Selection.StartPos;
end;

procedure TSXSkinCustomEdit.SetSelLength(Value: Integer);
var Selection: TSelection;
begin
 if FTextControl<>nil then
  begin
   SendMessage(FTextControl.Handle,EM_GETSEL,Longint(@Selection.StartPos),
       Longint(@Selection.EndPos));
   Selection.EndPos:=Selection.StartPos+Value;
   SendMessage(FTextControl.Handle,EM_SETSEL,Selection.StartPos,Selection.EndPos);
   SendMessage(FTextControl.Handle,EM_SCROLLCARET,0,0);
  end;
end;

procedure TSXSkinCustomEdit.Clear;
begin
 if FTextControl<>nil then
  SetWindowText(FTextControl.Handle,'');
end;

procedure TSXSkinCustomEdit.ClearSelection;
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,WM_CLEAR,0,0);
end;

procedure TSXSkinCustomEdit.CopyToClipboard;
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,WM_COPY,0,0);
end;

procedure TSXSkinCustomEdit.CutToClipboard;
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,WM_CUT,0,0);
end;

procedure TSXSkinCustomEdit.PasteFromClipboard;
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,WM_PASTE,0,0);
end;

procedure TSXSkinCustomEdit.Undo;
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,WM_UNDO,0,0);
end;

procedure TSXSkinCustomEdit.ClearUndo;
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,EM_EMPTYUNDOBUFFER,0,0);
end;

procedure TSXSkinCustomEdit.SelectAll;
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,EM_SETSEL,0,-1);
end;

function TSXSkinCustomEdit.GetSelTextBuf(Buffer:PChar;BufSize:Integer):Integer;
var     P:PChar;
 StartPos:Integer;
begin
 StartPos:=GetSelStart;
 Result:=GetSelLength;
 P:=StrAlloc(GetTextLen+1);
 try
  GetTextBuf(P,StrBufSize(P));
  if Result>=BufSize then Result:=BufSize-1;
  StrLCopy(Buffer,P+StartPos,Result);
 finally
  StrDispose(P);
 end;
end;

procedure TSXSkinCustomEdit.SetSelTextBuf(Buffer: PChar);
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,EM_REPLACESEL,0,LongInt(Buffer));
end;

function TSXSkinCustomEdit.GetSelText:String;
var SelStart,Len:Integer;
               P:PChar;
begin
 SelStart:=GetSelStart;
 Len:=GetSelLength;
 SetString(Result,PChar(nil),Len);
 if Len<>0 then
  begin
   P:=StrAlloc(GetTextLen+1);
   try
    GetTextBuf(P,StrBufSize(P));
    Move(P[SelStart],Pointer(Result)^,Len);
   finally
    StrDispose(P);
   end;
  end;
end;

procedure TSXSkinCustomEdit.SetSelText(const Value:String);
begin
 if FTextControl<>nil then
  SendMessage(FTextControl.Handle,EM_REPLACESEL,0,Longint(PChar(Value)));
end;

procedure TSXSkinCustomEdit.PaintCurrentEStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var    A:Integer;
  EState:TSXSkinEditStateParam;
   Style:TSXSkinGeneralStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentEState(EState);
   A:=SkinLibrary.Styles.GetGStyleIndex(EState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Style.DrawToBitmap(Self,CEID_Back,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,VComparer);
    end;
  end;
end;

procedure TSXSkinCustomEdit.PaintCurrentBlendedEStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var   BB:TBitmap32;
 CurEdit:TBitmap32;
    Rgn2:HRGN;
begin
 if HasTransformEffect(FLastEditTransform) and
    (FDoneSteps<FLastEditTransform.StepsNum) and (FLastEdit<>nil) then
  begin
   CurEdit:=TBitmap32.Create;
   BB:=TBitmap32.Create;
   try
    CurEdit.DrawMode:=dmBlend;
    CurEdit.CombineMode:=cmMerge;
    BB.DrawMode:=dmBlend;
    BB.CombineMode:=cmMerge;
    CurEdit.SetSize(Width,Height);
    CurEdit.Clear(0);
    Rgn2:=CreateRectRgn(0,0,Width,Height);
    PaintCurrentEStyle(CurEdit,0,0,Types.Rect(0,0,Width,Height),Rgn2);
    DeleteObject(Rgn2);
    BB.SetSize(Width,Height);
    ApplyTransformEffectToBitmaps(FLastEdit,CurEdit,FLastEditTransform,FDoneSteps,BB);
    BB.DrawTo(Bitmap,X-Rect.Left,Y-Rect.Top);
   finally
    BB.Free;
    CurEdit.Free;
   end;
  end else PaintCurrentEStyle(Bitmap,X,Y,Rect,Rgn);
end;

procedure TSXSkinCustomEdit.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   PaintCurrentBlendedEStyle(Bitmap,X,Y,Rect,Rgn);
  end;
 inherited;
end;

function TSXSkinCustomEdit.OnGetVariable(const VarName:String;var Error:Boolean):Single;
begin
 Result:=0;
 if VarName='W' then
  begin
   Result:=Width; exit;
  end;
 if VarName='H' then
  begin
   Result:=Height; exit;
  end;
 Error:=True;
end;

procedure TSXSkinCustomEdit.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
begin
 if not (csLoading in ComponentState) and FAutoSizeHeight then
  AHeight:=Height;
 inherited;
 if not (csLoading in ComponentState) then
  ResetEditParams([erpTextControl]);
end;

procedure TSXSkinCustomEdit.InternalSetBounds(ALeft,ATop,AWidth,AHeight:Integer);
begin
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

function TSXSkinCustomEdit.GetTabStop:Boolean;
begin
 if csDesigning in ComponentState then
  Result:=inherited TabStop else
   begin
    if FTextControl<>nil then
     Result:=FTextControl.TabStop else Result:=False;
   end;
end;

procedure TSXSkinCustomEdit.SetTabStop(Value:Boolean);
begin
 if csDesigning in ComponentState then
  inherited TabStop:=Value else
   begin
    if FTextControl<>nil then
     FTextControl.TabStop:=Value;
    inherited TabStop:=False;
   end;
end;

function TSXSkinCustomEdit.GetTabOrder:Integer;
begin
 if csDesigning in ComponentState then
  Result:=inherited TabOrder else
   begin
    if FTextControl<>nil then
     Result:=FTextControl.TabOrder else Result:=0;
   end;
end;

procedure TSXSkinCustomEdit.SetTabOrder(Value:Integer);
begin
 if csDesigning in ComponentState then
  inherited TabOrder:=Value else
   begin
    if FTextControl<>nil then
     FTextControl.TabOrder:=Value;
   end;
end;

procedure TSXSkinCustomEdit.SetAutoSizeHeight(Value:Boolean);
begin
 if Value<>FAutoSizeHeight then
  begin
   FAutoSizeHeight:=Value;
   if Value and not (csLoading in ComponentState) then
    ResetEditParams([erpTextControl]);
  end;
end;

procedure TSXSkinCustomEdit.GetCurrentEState(var EState:TSXSkinEditStateParam);
var  A:Integer;
 Style:TSXSkinEditStyle;

 procedure SetEStateFrom(const T:TSXSkinEditStateParam);
 begin
  if EState.Style='' then EState.Style:=T.Style;
  AddFontData(EState.FD,T.FD);
 end;

begin
 Finalize(EState);
 FillChar(EState,sizeof(EState),0);
 ClearFontData(EState.FD);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinEditStyle) then
    begin
     Style:=TSXSkinEditStyle(SkinLibrary.Styles[A]);
     if not Enabled then
      begin
       SetEStateFrom(Style.RUState);
       SetEStateFrom(Style.NUState);
      end else
     if FMouseOver then
      begin
       if FTextControl.FLastFocused then
        begin
         SetEStateFrom(Style.HFState);
         SetEStateFrom(Style.HUState);
         SetEStateFrom(Style.NFState);
         SetEStateFrom(Style.NUState);
        end else
         begin
          SetEStateFrom(Style.HUState);
          SetEStateFrom(Style.NUState);
         end;
      end else
     if FTextControl.FLastFocused then
      begin
       SetEStateFrom(Style.NFState);
       SetEStateFrom(Style.NUState);
      end else SetEStateFrom(Style.NUState);
    end;
  end;
 SetDefaultFontData(EState.FD,Font);
end;

procedure TSXSkinCustomEdit.InvalidateIfStyleChanged;
begin
 ResetEditParams([erpTextControlOnFontChange,erpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomEdit.InternalMouseEnter;
begin
 StartChanging(gcaHighlightIn);
 FMouseOver:=True;
 InvalidateIfStyleChanged;
 if Assigned(FOnMouseEnter) then
  FOnMouseEnter(Self);
end;

procedure TSXSkinCustomEdit.InternalMouseLeave;
begin
 if FMouseOver then
  begin
   StartChanging(gcaHighlightOut);
   FMouseOver:=False;
   InvalidateIfStyleChanged;
   if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  end;
end;

procedure TSXSkinCustomEdit.MouseLeave;
begin
 if FMouseOver then
  InternalMouseLeave;
 inherited;
end;

procedure TSXSkinCustomEdit.MouseMove(Shift:TShiftState;X,Y:Integer);
var B:Boolean;
    P:TPoint;
begin
 inherited;
 if Enabled then
  begin
   P:=Point(X,Y);
   B:=(X>=0) and (X<Width) and (Y>=0) and (Y<Height) and CapturesMouseAt(X,Y);
   if B<>FMouseOver then
    begin
     if B then InternalMouseEnter else
      InternalMouseLeave;
    end;
  end;
end;

procedure TSXSkinCustomEdit.StartChanging(T:TSXGlyphChangeAction);
var       B:TBitmap32;
        Rgn:HRGN;
 NeedThread:Boolean;
 ETransform:TSXTransformEffectData;
begin
 GetCurrentTransformEffect(T,ETransform);
 NeedThread:=HasTransformEffect(ETransform);
 if NeedThread then
  begin
   CreateThreadIfNeeded;
   B:=TBitmap32.Create;
   B.SetSize(Width,Height);
   B.Clear(0);
   Rgn:=CreateRectRgn(0,0,Width,Height);
   PaintCurrentBlendedEStyle(B,0,0,Rect(0,0,Width,Height),Rgn);
   DeleteObject(Rgn);
   FLastEdit.Free;
   FLastEdit:=B;
   if FLastEdit<>nil then
    begin
     FLastEdit.DrawMode:=dmBlend;
     FLastEdit.CombineMode:=cmMerge;
    end;
   FDoneSteps:=1;
   if FThread.Suspended then
    FThread.Resume;
  end else
   if (FThread<>nil) and not FThread.Suspended then
    begin
     FDoneSteps:=MaxInt;
     FThread.Suspend;
    end;
 FLastEditTransform:=ETransform;
end;

procedure TSXSkinCustomEdit.InvalidateEdit;
begin
 InvalidateRect(Handle,nil,False);
end;

procedure TSXSkinCustomEdit.DoThreadActions;
begin
 if not (csDestroying in ComponentState) then
  begin
   Inc(FDoneSteps);
   if (FThread<>nil) and not FThread.Suspended and
      (not HasTransformEffect(FLastEditTransform) or (FDoneSteps>=FLastEditTransform.StepsNum)) then
    begin
     FThread.Suspend;
    end;
   InvalidateEdit;
   Update;
   if FTextControl.HandleAllocated then
    begin
     InvalidateRect(FTextControl.Handle,nil,False);
     FTextControl.Update;
    end; 
  end;
end;

procedure TSXSkinCustomEdit.CreateThreadIfNeeded;
begin
 if FThread=nil then
  begin
   FThread:=TSXSkinCustomEditThread.Create;
   FThread.Control:=Self;
  end;
end;

procedure TSXSkinCustomEdit.CMFontChanged(var Message:TMessage);
begin
 inherited;
 if not (csLoading in ComponentState) then
  ResetEditParams([erpTextControlOnFontChange]);
end;

procedure TSXSkinCustomEdit.ResetEditParams(Params:TSXSkinEditResetParams=[]);
var  EState:TSXSkinEditStateParam;
  SetEState:Boolean;
    Changed:Boolean;
     EStyle:TSXSkinEditStyle;
     A,H,TH:Integer;
 NewTCBRect:TRect;
   NewBRect:TRect;

 procedure DoSetEState;
 begin
  if not SetEState then
   begin
    SetEState:=True;
    GetCurrentEState(EState);
   end;
 end;

begin
 SetEState:=False;
 if Params*[erpTextControl,erpTextControlOnFontChange]<>[] then
  begin
   DoSetEState;
   if erpTextControl in Params then Changed:=True else
    Changed:=not SameFontData(EState.FD,FLastFontData);
   FLastFontData:=EState.FD;
   if Changed then
    begin
     FTextControl.Font.Name:=EState.FD.FontName;
     FTextControl.Font.Size:=EState.FD.FontSize;
     FTextControl.Font.Style:=EState.FD.FontStyle;
     FTextControl.Font.Color:=WinColor(EState.FD.FontColor);
     Canvas.Font:=FTextControl.Font;
     if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
      begin
       A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
       if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinEditStyle) then
        begin
         EStyle:=TSXSkinEditStyle(SkinLibrary.Styles[A]);
         TH:=Canvas.TextHeight('1');
         if FAutoSizeHeight then
          begin
           NewBRect:=Rect(0,0,Width,EStyle.TextTopOffset+EStyle.TextBottomOffset+TH);
           OffsetRect(NewBRect,Left,Top);
           if not EqualRect(NewBRect,BoundsRect) then
            InternalSetBounds(NewBRect.Left,NewBRect.Top,
                 NewBRect.Right-NewBRect.Left,NewBRect.Bottom-NewBRect.Top);
          end;
         H:=Height-EStyle.TextTopOffset-EStyle.TextBottomOffset;
         NewTCBRect:=Rect(EStyle.TextLeftOffset,EStyle.TextTopOffset+(H-TH) div 2,
          Width-EStyle.TextRightOffset,EStyle.TextTopOffset+(H+TH) div 2);
         if not EqualRect(FTextControl.BoundsRect,NewTCBRect) then
          FTextControl.BoundsRect:=NewTCBRect;
        end;
      end;
    end;
  end;
 if erpInvalidateOnStyleChange in Params then
  begin
   DoSetEState;
   if EState.Style<>FLastStyle then
    begin
     FLastStyle:=EState.Style;
     InvalidateEdit;
     Update;
    end;
  end;
end;

procedure TSXSkinCustomEdit.Loaded;
begin
 inherited;
 ResetEditParams([erpTextControl,erpInvalidateOnStyleChange]);
 SendMessage(FTextControl.Handle,EM_SETMARGINS,EC_LEFTMARGIN or EC_RIGHTMARGIN,0);
end;

procedure TSXSkinCustomEdit.SkinChanged;
begin
 if not (csLoading in ComponentState) then
  begin
   FLastEdit.Free;
   FLastEdit:=nil;
   if (FThread<>nil) and not FThread.Suspended then
    FThread.Suspend;
   ResetEditParams([erpTextControl,erpInvalidateOnStyleChange]);
  end;
 inherited;
end;

procedure TSXSkinCustomEdit.WMCtlColorEdit(var Msg:TMessage);
var B:TBitmap32;
  Rgn:HRGN;
    R:TRect;
   PC:TControl;
begin
 B:=TBitmap32.Create;
 try
  B.SetSize(FTextControl.Width,FTextControl.Height);
  B.DrawMode:=dmBlend;
  B.CombineMode:=cmMerge;
  Rgn:=CreateRectRgnIndirect(FTextControl.BoundsRect);
  R:=FTextControl.BoundsRect;
  PC:=Parent;
  while (PC<>nil) and (PC is TSXSkinCustomControl) and (PC.Parent<>nil) and
        (PC.Parent is TSXSkinCustomControl) do
   begin
    OffsetRect(R,PC.Left,PC.Top);
    OffsetRgn(Rgn,PC.Left,PC.Top);
    PC:=PC.Parent;
   end;
  ControlsNotToPaint.Add(FTextControl);
  if (PC<>nil) and (PC is TSXSkinCustomControl) then
   begin
    OffsetRgn(Rgn,Left,Top);
    OffsetRect(R,Left,Top);
    TSXSkinCustomControl(PC).PaintRectToBitmap(HDC(Msg.WParam),FTextControl.ClientRect,R,Rgn,B,0,0,True);
   end else PaintRectToBitmap(HDC(Msg.WParam),FTextControl.ClientRect,R,Rgn,B,0,0,False);
  ControlsNotToPaint.Remove(FTextControl);
  DeleteObject(Rgn);
  FBrushBitmap.Width:=B.Width;
  FBrushBitmap.Height:=B.Height;
  B.DrawTo(FBrushBitmap.Canvas.Handle,0,0);
  FBackBrush.Bitmap:=nil;
  FBackBrush.Bitmap:=FBrushBitmap;
  Msg.Result:=Integer(FBackBrush.Handle);
  SetTextColor(HDC(Msg.WParam),ColorToRGB(FTextControl.Font.Color));
  SetBkMode(HDC(Msg.WParam),TRANSPARENT);
  if FTextControl.FFontChanged then
   begin
    FTextControl.FFontChanged:=False;
    SendMessage(FTextControl.Handle,EM_SETMARGINS,EC_LEFTMARGIN or EC_RIGHTMARGIN,0);
   end;
 finally
  B.Free;
 end;
end;

procedure TSXSkinCustomEdit.WMCtlColorStatic(var Msg:TMessage);
begin
 WMCtlColorEdit(Msg);
end;

procedure TSXSkinCustomEdit.GetCurrentTransformEffect(Action:TSXGlyphChangeAction;
           var Effect:TSXTransformEffectData);
var    A:Integer;
  EStyle:TSXSkinEditStyle;
begin
 FillChar(Effect,sizeof(Effect),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinEditStyle) then
    begin
     EStyle:=TSXSkinEditStyle(SkinLibrary.Styles[A]);
     case Action of
      gcaHighlightIn:  Effect:=EStyle.HInEditEffect;
      gcaHighlightOut: Effect:=EStyle.HOutEditEffect;
      gcaEnable:       Effect:=EStyle.EnableEditEffect;
      gcaDisable:      Effect:=EStyle.DisableEditEffect;
      gcaFocus:        Effect:=EStyle.FocusEditEffect;
      gcaUnfocus:      Effect:=EStyle.UnfocusEditEffect;
     end;
    end;
  end;
end;

function TSXSkinCustomEdit.GetText:String;
begin
 Result:=FTextControl.Text;
end;

procedure TSXSkinCustomEdit.SetText(const Value:String);
begin
 FTextControl.Text:=Value;
end;

function TSXSkinCustomEdit.IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean;
var   A:Integer;
  Style:TSXSkinGeneralStyle;
 EState:TSXSkinEditStateParam;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentEState(EState);
   A:=SkinLibrary.Styles.GetGStyleIndex(EState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
    end;
   if Result and (FTextControl<>nil) then
    Result:=not PtInRect(FTextControl.BoundsRect,Point(X,Y));
  end;
end;

function TSXSkinCustomEdit.CapturesMouseAt(X,Y:Integer):Boolean;
var   A:Integer;
  Style:TSXSkinGeneralStyle;
 EState:TSXSkinEditStateParam;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentEState(EState);
   A:=SkinLibrary.Styles.GetGStyleIndex(EState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
    end;
   if not Result then
    Result:=PtInRect(FTextControl.BoundsRect,Point(X,Y));
  end;
end;

constructor TSXSkinCustomEdit.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 CEID_Back:=GetNewCElementID;
 VComparer:=TSXEditVariableComparer.Create;
 VComparer.Control:=Self;
 VComparer.OnGetVariable:=OnGetVariable;
 FBackBrush:=TBrush.Create;
 FBrushBitmap:=TBitmap.Create;
 ControlStyle:=[csClickEvents,csSetCaption,csDoubleClicks{$IFDEF COMPILER_10_UP},csPannable{$ENDIF}];
 FTextControl:=TSXSkinCustomEditTextControl.Create(Self);
 FTextControl.Parent:=Self;
 FTextControl.BoundsRect:=Rect(4,4,Width-4,Height-4);
 ParentColor:=False;
 FAutoSelect:=True;
 FAutoSizeHeight:=True;
 FHideSelection:=True;
 Width:=121;
 Height:=25;
 TabStop:=True;
 SkinStyle:='_Edit';
end;

destructor TSXSkinCustomEdit.Destroy;
begin
 FBackBrush.Free;
 FBrushBitmap.Free;
 FLastEdit.Free;
 FThread.Free;
 inherited;
 VComparer.Free; 
end;

end.
