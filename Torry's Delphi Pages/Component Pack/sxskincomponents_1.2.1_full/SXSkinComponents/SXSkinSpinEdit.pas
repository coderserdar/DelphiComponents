unit SXSkinSpinEdit;

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
     SysUtils, Graphics, GR32, SXSkinLibrary, GR32_Blend, SXSkinEdit,
     SXSkinUpDown, Types;

type

 TSXSkinSpinEditButtonsPosition=(bpLeft,bpRight);

 TSXSkinCustomSpinEdit=class(TSXSkinCustomControl)
  private
   FEditRect:TRect;
   FHorizontal:Boolean;
   FButtonsPosition:TSXSkinSpinEditButtonsPosition;
   FUpDownRect:TRect;
   FSkinEdit:TSXSkinCustomEdit;
   FSkinUpDown:TSXSkinCustomUpDown;
   FMinValue:Integer;
   FMaxValue:Integer;
   FIncrement:Integer;
   FEditorEnabled:Boolean;
   FOnChange:TNotifyEvent;   
   FOnUserModified:TNotifyEvent;
   FOnMouseDown:TMouseEvent;
   FOnMouseMove:TMouseMoveEvent;
   FOnMouseUp:TMouseEvent;
   FOnDragDrop:TDragDropEvent;
   FOnDragOver:TDragOverEvent;
   FOnStartDock:TStartDockEvent;
   FOnEndDock:TEndDragEvent;
   FOnStartDrag:TStartDragEvent;
   FOnEndDrag:TEndDragEvent;
   FOnClick:TNotifyEvent;
   FOnDblClick:TNotifyEvent;
   FOnContextPopup:TContextPopupEvent;
   FOnMouseLeave:TNotifyEvent;
   FOnMouseEnter:TNotifyEvent;
   FOnMouseWheel:TMouseWheelEvent;
   FOnMouseWheelDown:TMouseWheelUpDownEvent;
   FOnMouseWheelUp:TMouseWheelUpDownEvent;
   FLastChangeValue:String;
   function GetAlignment:TAlignment;
   procedure SetAlignment(Value:TAlignment);
   function GetAutoSizeHeight:Boolean;
   procedure SetAutoSizeHeight(Value:Boolean);
   function GetMaxLength:Integer;
   procedure SetMaxLength(Value:Integer);
   procedure SetMaxValue(Value:Integer);
   procedure SetMinValue(Value:Integer);
   function GetValue:Integer;
   procedure SetValue(AValue:Integer);
   function GetReadOnly:Boolean;
   procedure SetReadOnly(Value:Boolean);
   function GetText:String;
   procedure SetText(const Value:String);
   procedure SetButtonsPosition(Value:TSXSkinSpinEditButtonsPosition);
   procedure SetHorizontal(Value:Boolean);
   procedure ResetChildrenStyles;
   procedure ResetChildrenPosition;
   function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   function HasUnusualSkinStyle:Boolean;
   procedure AdjustToEditBounds;
   procedure SkinEditResize(Sender:TObject);
   function CheckValueRanges(Value:Integer):Integer;
   procedure CheckCurrentValue;
   function IsValidChar(Key:Char):Boolean;
   procedure EditKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
   procedure EditKeyPress(Sender:TObject;var Key:Char);
   procedure EditUsedModified(Sender:TObject);
   procedure UpDownClick(Sender:TObject;UpButton:Boolean);
   procedure EditClick(Sender:TObject);
   procedure EditContextPopup(Sender:TObject;MousePos:TPoint;var Handled:Boolean);
   procedure EditDblClick(Sender:TObject);
   procedure EditDragDrop(Sender,Source:TObject;X,Y:Integer);
   procedure EditDragOver(Sender,Source:TObject;X,Y:Integer;State:TDragState;var Accept:Boolean);
   procedure EditEndDock(Sender,Target:TObject;X,Y:Integer);
   procedure EditEndDrag(Sender,Target:TObject;X,Y:Integer);
   procedure EditMouseDown(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
   procedure EditMouseEnter(Sender:TObject);
   procedure EditMouseLeave(Sender:TObject);
   procedure EditMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
   procedure EditMouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
   procedure EditMouseWheel(Sender:TObject;Shift:TShiftState;WheelDelta:Integer;
              MousePos:TPoint;var Handled:Boolean);
   procedure EditMouseWheelDown(Sender:TObject;Shift:TShiftState;MousePos:TPoint;
              var Handled:Boolean);
   procedure EditMouseWheelUp(Sender:TObject;Shift:TShiftState;MousePos:TPoint;
              var Handled:Boolean);
   procedure EditStartDock(Sender:TObject;var DragObject:TDragDockObject);
   procedure EditStartDrag(Sender:TObject;var DragObject:TDragObject);
  protected
   procedure Loaded; override;
   procedure SetEnabled(Value:Boolean); override;
  public
   procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
   procedure SkinChanged; override;
   property Alignment:TAlignment read GetAlignment write SetAlignment default taLeftJustify;
   property AutoSizeHeight:Boolean read GetAutoSizeHeight write SetAutoSizeHeight default True;
   property ButtonsPosition:TSXSkinSpinEditButtonsPosition read FButtonsPosition
              write SetButtonsPosition default bpRight;
   property EditorEnabled:Boolean read FEditorEnabled write FEditorEnabled default True;
   property Increment:Integer read FIncrement write FIncrement default 1;
   property Horizontal:Boolean read FHorizontal write SetHorizontal default False;
   property MaxLength:Integer read GetMaxLength write SetMaxLength default 0;
   property MaxValue:Integer read FMaxValue write SetMaxValue default -1;
   property MinValue:Integer read FMinValue write SetMinValue default 0;
   property ReadOnly:Boolean read GetReadOnly write SetReadOnly default False;
   property SkinStyle stored HasUnusualSkinStyle;
   property Text:String read GetText write SetText;
   property Value:Integer read GetValue write SetValue;
   property OnChange:TNotifyEvent read FOnChange write FOnChange;
   property OnUserModified:TNotifyEvent read FOnUserModified write FOnUserModified;
   property OnClick:TNotifyEvent read FOnClick write FOnClick;
   property OnContextPopup:TContextPopupEvent read FOnContextPopup write FOnContextPopup;
   property OnDblClick:TNotifyEvent read FOnDblClick write FOnDblClick;
   property OnDragDrop:TDragDropEvent read FOnDragDrop write FOnDragDrop;
   property OnDragOver:TDragOverEvent read FOnDragOver write FOnDragOver;
   property OnEndDock:TEndDragEvent read FOnEndDock write FOnEndDock;
   property OnEndDrag:TEndDragEvent read FOnEndDrag write FOnEndDrag;
   property OnMouseDown:TMouseEvent read FOnMouseDown write FOnMouseDown;
   property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   property OnMouseMove:TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
   property OnMouseUp:TMouseEvent read FOnMouseUp write FOnMouseUp;
   property OnMouseWheel:TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
   property OnMouseWheelDown:TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
   property OnMouseWheelUp:TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
   property OnStartDock:TStartDockEvent read FOnStartDock write FOnStartDock;
   property OnStartDrag:TStartDragEvent read FOnStartDrag write FOnStartDrag;
   constructor Create(AOwner:TComponent); override;
 end;

 TSXSkinSpinEdit=class(TSXSkinCustomSpinEdit)
  published
   property Align;
   property Alignment;
   property Anchors;
   property AutoSizeHeight;
   property ButtonsPosition;
   property Constraints;
   property Cursor;
   property DragCursor;
   property EditorEnabled;
   property Enabled;
   property Font;
   //property HintData;
   property Horizontal;
   property Increment;
   property MaxLength;
   property MaxValue;
   property MinValue;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ReadOnly;
   property ShowHint;
   property SkinLibrary;
   property SkinStyle;
   property Text;
   property Value;
   property Visible;
   property OnChange;
   property OnUserModified;
   property OnClick;
   property OnContextPopup;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDock;
   property OnEndDrag;
   property OnMouseDown;
   property OnMouseEnter;
   property OnMouseLeave;
   property OnMouseMove;
   property OnMouseUp;
   property OnMouseWheel;
   property OnMouseWheelDown;
   property OnMouseWheelUp;
   property OnResize;
   property OnStartDock;
   property OnStartDrag;
 end;

implementation

{ TSXSkinCustomSpinEdit }

function TSXSkinCustomSpinEdit.HasUnusualSkinStyle:Boolean;
begin
 Result:=SkinStyle<>'_SpinEditRV';
end;

function TSXSkinCustomSpinEdit.GetAlignment:TAlignment;
begin
 Result:=FSkinEdit.Alignment;
end;

procedure TSXSkinCustomSpinEdit.SetAlignment(Value:TAlignment);
begin
 FSkinEdit.Alignment:=Value;
end;

function TSXSkinCustomSpinEdit.GetAutoSizeHeight:Boolean;
begin
 Result:=FSkinEdit.AutoSizeHeight;
end;

procedure TSXSkinCustomSpinEdit.SetAutoSizeHeight(Value:Boolean);
begin
 FSkinEdit.AutoSizeHeight:=Value;
end;

function TSXSkinCustomSpinEdit.GetMaxLength:Integer;
begin
 Result:=FSkinEdit.MaxLength;
end;

procedure TSXSkinCustomSpinEdit.SetMaxLength(Value:Integer);
begin
 FSkinEdit.MaxLength:=Value;
end;

function TSXSkinCustomSpinEdit.GetText:String;
begin
 Result:=FSkinEdit.Text;
end;

procedure TSXSkinCustomSpinEdit.SetText(const Value:String);
begin
 FSkinEdit.Text:=Value;
 CheckCurrentValue;
 if Assigned(OnChange) and (FLastChangeValue<>Text) then
  begin
   FLastChangeValue:=Text;
   OnChange(Self);
  end;
end;

procedure TSXSkinCustomSpinEdit.SetMaxValue(Value:Integer);
begin
 if FMaxValue<>Value then
  begin
   FMaxValue:=Value;
   CheckCurrentValue;
  end;
end;

procedure TSXSkinCustomSpinEdit.SetMinValue(Value:Integer);
begin
 if FMinValue<>Value then
  begin
   FMinValue:=Value;
   CheckCurrentValue;
  end;
end;

function TSXSkinCustomSpinEdit.GetValue:Integer;
begin
 Result:=StrToIntDef(Text,0);
end;

procedure TSXSkinCustomSpinEdit.SetValue(AValue:Integer);
begin
 AValue:=CheckValueRanges(AValue);
 if AValue<>Value then
  Text:=inttostr(AValue);
end;

function TSXSkinCustomSpinEdit.GetReadOnly:Boolean;
begin
 Result:=FSkinEdit.ReadOnly;
end;

procedure TSXSkinCustomSpinEdit.SetReadOnly(Value:Boolean);
begin
 FSkinEdit.ReadOnly:=Value;
end;

function TSXSkinCustomSpinEdit.IsValidChar(Key:Char):Boolean;
begin
 Result:=(Key in ['-','0'..'9']) or ((Key < #32) and (Key<>Chr(VK_RETURN)));
 if not FEditorEnabled and Result and ((Key>=#32) or (Key=Char(VK_BACK)) or (Key=Char(VK_DELETE))) then
  Result:=False;
end;

procedure TSXSkinCustomSpinEdit.SetButtonsPosition(Value:TSXSkinSpinEditButtonsPosition);
begin
 if FButtonsPosition<>Value then
  begin
   FButtonsPosition:=Value;
   if not (csLoading in ComponentState) then
    begin
     if (FButtonsPosition=bpLeft) and (SkinStyle='_SpinEditRV') then
      SkinStyle:='_SpinEditLV' else
     if (FButtonsPosition=bpLeft) and (SkinStyle='_SpinEditRH') then
      SkinStyle:='_SpinEditLH' else
     if (FButtonsPosition=bpRight) and (SkinStyle='_SpinEditLV') then
      SkinStyle:='_SpinEditRV' else
     if (FButtonsPosition=bpRight) and (SkinStyle='_SpinEditLH') then
      SkinStyle:='_SpinEditRH';
    end;
  end;
end;

procedure TSXSkinCustomSpinEdit.SetHorizontal(Value:Boolean);
begin
 if FHorizontal<>Value then
  begin
   FHorizontal:=Value;
   if not (csLoading in ComponentState) then
    begin
     if FHorizontal and (SkinStyle='_SpinEditRV') then
      SkinStyle:='_SpinEditRH' else
     if Horizontal and (SkinStyle='_SpinEditLV') then
      SkinStyle:='_SpinEditLH' else
     if not FHorizontal and (SkinStyle='_SpinEditRH') then
      SkinStyle:='_SpinEditRV' else
     if not Horizontal and (SkinStyle='_SpinEditLH') then
      SkinStyle:='_SpinEditLV';
    end;
  end;
end;

function TSXSkinCustomSpinEdit.CheckValueRanges(Value:Integer):Integer;
begin
 Result:=Value;
 if FMaxValue>=FMinValue then
  begin
   if Result>FMaxValue then
    Result:=FMaxValue else
   if Result<FMinValue then
    Result:=FMinValue;
  end;
end;

procedure TSXSkinCustomSpinEdit.CheckCurrentValue;
var V1,V2:Integer;
begin
 V1:=Value;
 V2:=CheckValueRanges(V1);
 if (V1<>V2) or ((V1=0) and (Text<>'0')) then
  Text:=inttostr(V2);
end;

procedure TSXSkinCustomSpinEdit.AdjustToEditBounds;
begin
 if not EqualRect(FSkinEdit.BoundsRect,FEditRect) and (FSkinEdit.Height>0) and
        (FEditRect.Bottom-FEditRect.Top>0) then
  begin
   Height:=Height+FSkinEdit.Height-FEditRect.Bottom+FEditRect.Top;
  end;
end;

procedure TSXSkinCustomSpinEdit.ResetChildrenStyles;
var  A:Integer;
 Style:TSXSkinSpinEditStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinSpinEditStyle) then
    begin
     Style:=TSXSkinSpinEditStyle(SkinLibrary.Styles[A]);
     FSkinEdit.SkinStyle:=Style.Edit;
     FSkinEdit.SkinLibrary:=SkinLibrary;
     FSkinUpDown.SkinStyle:=Style.UpDown;
     FSkinUpDown.SkinLibrary:=SkinLibrary;
    end;
  end;
end;

procedure TSXSkinCustomSpinEdit.ResetChildrenPosition;
var  A:Integer;
 Style:TSXSkinSpinEditStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinSpinEditStyle) then
    begin
     Style:=TSXSkinSpinEditStyle(SkinLibrary.Styles[A]);
     GetRectFromString(Style.EditRect,FEditRect,OnGetVariable);
     GetRectFromString(Style.UpDownRect,FUpDownRect,OnGetVariable);
     FSkinUpDown.BoundsRect:=FUpDownRect;
     FSkinEdit.BoundsRect:=FEditRect;
    end;
  end;
end;

function TSXSkinCustomSpinEdit.OnGetVariable(const VarName:String;var Error:Boolean):Single;
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

procedure TSXSkinCustomSpinEdit.SkinChanged;
begin
 if not (csLoading in ComponentState) then
  begin
   ResetChildrenPosition;
   ResetChildrenStyles;
   FSkinEdit.SkinChanged;
   FSkinUpDown.SkinChanged;
  end;
 inherited;
end;

procedure TSXSkinCustomSpinEdit.Loaded;
begin
 inherited;
 ResetChildrenPosition;
 ResetChildrenStyles;
 FSkinEdit.SetLoaded;
 FSkinUpDown.SetLoaded; 
end;

procedure TSXSkinCustomSpinEdit.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
var OldWidth,OldHeight:Integer;
begin
 OldWidth:=Width;
 OldHeight:=Height;
 inherited;
 if not (csLoading in ComponentState) and ((Width<>OldWidth) or (Height<>OldHeight)) then
  begin
   ResetChildrenPosition;
   AdjustToEditBounds; 
  end;
end;

procedure TSXSkinCustomSpinEdit.SkinEditResize(Sender:TObject);
begin
 AdjustToEditBounds;
end;

procedure TSXSkinCustomSpinEdit.EditKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if Key=VK_UP then UpDownClick(Self,True) else
  if Key=VK_DOWN then UpDownClick(Self,False);
end;

procedure TSXSkinCustomSpinEdit.EditKeyPress(Sender:TObject;var Key:Char);
begin
 if not IsValidChar(Key) then
  begin
   Key:=#0;
   MessageBeep(0)
  end;
end;

procedure TSXSkinCustomSpinEdit.EditUsedModified(Sender:TObject);
begin
 CheckCurrentValue;
 if Assigned(OnChange) and (FLastChangeValue<>Text) then
  begin
   FLastChangeValue:=Text;
   OnChange(Self);
  end;
 if Assigned(OnUserModified) then
  OnUserModified(Self);
end;

procedure TSXSkinCustomSpinEdit.UpDownClick(Sender:TObject;UpButton:Boolean);
var LCV:String;
begin
 LCV:=Text;
 if ReadOnly then MessageBeep(0) else
 if UpButton then
  Value:=Value+FIncrement else
   Value:=Value-FIncrement;
 if Text<>LCV then
  begin
   if Assigned(OnUserModified) then
    OnUserModified(Self);
  end;
end;

procedure TSXSkinCustomSpinEdit.EditClick(Sender:TObject);
begin
 if Assigned(OnClick) then
  OnClick(Self);
end;

procedure TSXSkinCustomSpinEdit.EditContextPopup(Sender:TObject;MousePos:TPoint;var Handled:Boolean);
begin
 if Assigned(OnContextPopup) then
  OnContextPopup(Self,MousePos,Handled);
end;

procedure TSXSkinCustomSpinEdit.EditDblClick(Sender:TObject);
begin
 if Assigned(OnDblClick) then
  OnDblClick(Self);
end;

procedure TSXSkinCustomSpinEdit.EditDragDrop(Sender,Source:TObject;X,Y:Integer);
begin
 if Assigned(OnDragDrop) then
  OnDragDrop(Self,Source,X,Y);
end;

procedure TSXSkinCustomSpinEdit.EditDragOver(Sender,Source:TObject;X,Y:Integer;
           State:TDragState;var Accept:Boolean);
begin
 if Assigned(OnDragOver) then
  OnDragOver(Self,Source,X,Y,State,Accept);
end;

procedure TSXSkinCustomSpinEdit.EditEndDock(Sender,Target:TObject;X,Y:Integer);
begin
 if Assigned(OnEndDock) then
  OnEndDock(Self,Target,X,Y);
end;

procedure TSXSkinCustomSpinEdit.EditEndDrag(Sender,Target:TObject;X,Y:Integer);
begin
 if Assigned(OnEndDrag) then
  OnEndDrag(Self,Target,X,Y);
end;

procedure TSXSkinCustomSpinEdit.EditMouseDown(Sender:TObject;Button:TMouseButton;
           Shift:TShiftState;X,Y:Integer);
begin
 if Assigned(OnMouseDown) then
  OnMouseDown(Self,Button,Shift,X,Y);
end;

procedure TSXSkinCustomSpinEdit.EditMouseEnter(Sender:TObject);
begin
 if Assigned(OnMouseEnter) then
  OnMouseEnter(Self);
end;

procedure TSXSkinCustomSpinEdit.EditMouseLeave(Sender:TObject);
begin
 if Assigned(OnMouseLeave) then
  OnMouseLeave(Self);
end;

procedure TSXSkinCustomSpinEdit.EditMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
begin
 if Assigned(OnMouseMove) then
  OnMouseMove(Self,Shift,X,Y);
end;

procedure TSXSkinCustomSpinEdit.EditMouseUp(Sender:TObject;Button:TMouseButton;
           Shift:TShiftState;X,Y:Integer);
begin
 if Assigned(OnMouseUp) then
  OnMouseUp(Self,Button,Shift,X,Y);
end;

procedure TSXSkinCustomSpinEdit.EditMouseWheel(Sender:TObject;Shift:TShiftState;
           WheelDelta:Integer;MousePos:TPoint;var Handled:Boolean);
begin
 if Assigned(OnMouseWheel) then
  OnMouseWheel(Self,Shift,WheelDelta,MousePos,Handled);
end;

procedure TSXSkinCustomSpinEdit.EditMouseWheelDown(Sender:TObject;Shift:TShiftState;
           MousePos:TPoint;var Handled:Boolean);
begin
 if Assigned(OnMouseWheelDown) then
  OnMouseWheelDown(Self,Shift,MousePos,Handled);
end;

procedure TSXSkinCustomSpinEdit.EditMouseWheelUp(Sender:TObject;Shift:TShiftState;
           MousePos:TPoint;var Handled:Boolean);
begin
 if Assigned(OnMouseWheelUp) then
  OnMouseWheelUp(Self,Shift,MousePos,Handled);
end;

procedure TSXSkinCustomSpinEdit.EditStartDock(Sender:TObject;var DragObject:TDragDockObject);
begin
 if Assigned(OnStartDock) then
  OnStartDock(Self,DragObject);
end;

procedure TSXSkinCustomSpinEdit.EditStartDrag(Sender:TObject;var DragObject:TDragObject);
begin
 if Assigned(OnStartDrag) then
  OnStartDrag(Self,DragObject);
end;

procedure TSXSkinCustomSpinEdit.SetEnabled(Value:Boolean);
begin
 if Enabled<>Value then
  begin
   inherited;
   FSkinEdit.Enabled:=Value;
   FSkinUpDown.Enabled:=Value;
  end;
end;

constructor TSXSkinCustomSpinEdit.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FSkinEdit:=TSXSkinCustomEdit.Create(Self);
 FSkinEdit.Parent:=Self;
 FSkinEdit.OnResize:=SkinEditResize;
 FSkinEdit.OnKeyDown:=EditKeyDown;
 FSkinEdit.OnKeyPress:=EditKeyPress;
 FSkinEdit.OnUserModified:=EditUsedModified;
 FSkinEdit.OnClick:=EditClick;
 FSkinEdit.OnContextPopup:=EditContextPopup;
 FSkinEdit.OnDblClick:=EditDblClick;
 FSkinEdit.OnDragDrop:=EditDragDrop;
 FSkinEdit.OnDragOver:=EditDragOver;
 FSkinEdit.OnEndDock:=EditEndDock;
 FSkinEdit.OnEndDrag:=EditEndDrag;
 FSkinEdit.OnMouseDown:=EditMouseDown;
 FSkinEdit.OnMouseEnter:=EditMouseEnter;
 FSkinEdit.OnMouseLeave:=EditMouseLeave;
 FSkinEdit.OnMouseMove:=EditMouseMove;
 FSkinEdit.OnMouseUp:=EditMouseUp;
 FSkinEdit.OnMouseWheel:=EditMouseWheel;
 FSkinEdit.OnMouseWheelDown:=EditMouseWheelDown;
 FSkinEdit.OnMouseWheelUp:=EditMouseWheelUp;
 FSkinEdit.OnStartDock:=EditStartDock;
 FSkinEdit.OnStartDrag:=EditStartDrag;
 FSkinEdit.Text:='0';
 //
 FSkinUpDown:=TSXSkinCustomUpDown.Create(Self);
 FSkinUpDown.Parent:=Self;
 FSkinUpDown.OnClick:=UpDownClick;
 FSkinUpDown.OnContextPopup:=EditContextPopup;
 FSkinUpDown.OnDragDrop:=EditDragDrop;
 FSkinUpDown.OnDragOver:=EditDragOver;
 FSkinUpDown.OnEndDock:=EditEndDock;
 FSkinUpDown.OnEndDrag:=EditEndDrag;
 FSkinUpDown.OnMouseDown:=EditMouseDown;
 FSkinUpDown.OnMouseEnter:=EditMouseEnter;
 FSkinUpDown.OnMouseLeave:=EditMouseLeave;
 FSkinUpDown.OnMouseMove:=EditMouseMove;
 FSkinUpDown.OnMouseUp:=EditMouseUp;
 FSkinUpDown.OnMouseWheel:=EditMouseWheel;
 FSkinUpDown.OnMouseWheelDown:=EditMouseWheelDown;
 FSkinUpDown.OnMouseWheelUp:=EditMouseWheelUp;
 FSkinUpDown.OnStartDock:=EditStartDock;
 FSkinUpDown.OnStartDrag:=EditStartDrag;
 //
 FButtonsPosition:=bpRight;
 FEditorEnabled:=True;
 FIncrement:=1;
 FMaxValue:=-1;
 FMinValue:=0;
 SkinStyle:='_SpinEditRV';
end;

end.
