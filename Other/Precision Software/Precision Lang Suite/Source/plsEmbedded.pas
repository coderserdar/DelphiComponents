{------------------------------------------------------------------------------
  plsEmbedded.pas

  Precision Language Suite

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Embedded localization editor, that allows you to write your
              language dependent texts directly in the running application

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.

  Copyright (c) 2008 - 2014  Precision software & consulting
  All rights reserved

------------------------------------------------------------------------------
  History:

  - Version: 2.5.2
    * added: Support for Delphi XE6 and XE7

  - Version: 2.5.1
    * added: support for Delphi XE3, XE4, XE5
    * improved: The unit has been redesigned to provide one source for all supported platforms (VCL, LCL)
    * changed: support for Delphi 5 & 6 has been dropped
    * and other minor improvements ...

  - Version: 2.2.8
    * changed - Minor improvements

  - Version: 2.2.7
    * added - support for inline frames

  - Version: 2.2.5.2
    * fixed - function FindControlAtPos for 3rd and higher level of child controls
    * fixed - compatibility with Delphi 5,6, and 7

  - Version: 2.2.4.19
    * added - Support for compressed language files format (lngz)
------------------------------------------------------------------------------}

{ Embedded localization editor, that allows you to write
  your language dependent texts directly in the running application }

unit plsEmbedded;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, plsLangMan, Grids, ExtCtrls, {$IFDEF FPC} lresources, lclproc {$ELSE} Menus {$ENDIF};

type
  { Basic editor for an embedded localization. Please, do not create this form directly,
    rather use EmbeddedEditor, LE_BeginCaptureControl or LE_InstallCaptureControlShortcut procedures. }
  TfrmEmbeddedEditor = class(TForm)
    pBottom: TPanel;
    sbOK: TButton;
    sbCancel: TButton;
    sgItems: TStringGrid;
    procedure sgItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgItemsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure FormResize(Sender: TObject);
    procedure sgItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgItemsDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure sgItemsGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
  private
    { Private declarations }
    // Language manager to use
    FLangMan:TLanguageManager;
    // Root (owner) localized component
    FRoot:TComponent;
    // Initial component filter
    FOnlyComponent:TComponent;
    FOldValue:string;
    FModified:Boolean;
    // Fills the editor grid with desired properties and constants that are available to localize
    procedure PrepareItems(OnlyPredefinedProps:Boolean=False; IncludeConstants:Boolean=False);
    // Saves the changes into an appropriate language files (or creates a new file if needed)
    procedure SaveItems;
  public
    { Public declarations }
  end;

// Installs the keyboard shortcut for starting the capturing of components, that will be selected for localization
procedure LE_InstallCaptureControlShortcut(const LangMan:TLanguageManager; Shortcut:TShortCut);
// Removes the keyboard shortcut previously installed by LE_InstallCaptureControlShortcut procedure
procedure LE_RemoveCaptureControlShortcut;

{ This procedure starts capturing of components, that will be selected for localization on currently active form }
procedure LE_BeginCaptureControl(const LangMan:TLanguageManager);
{ Stops capturing of components, previously started by LE_BeginCaptureControl procedure }
procedure LE_EndCaptureControl;

{ Invokes an embedded localization editor. You can call this method from your code, or you can use LE_BeginCaptureControl procedure,
  or you can install a capture control shortcut by LE_InstallCaptureControlShortcut procedure. }
procedure EmbeddedEditor(const LangMan:TLanguageManager; const Owner:TComponent; const OnlyComponent:TComponent=nil;
  OnlyPredefinedProps:Boolean=False; IncludeConstants:Boolean=False; Title:string='');

var
  // Use this variable to define the title of "embedded localization editor" (see EmbeddedEditor)
  pls_Editor_Title:string;
  // Defines a width of "embedded localization editor" (see EmbeddedEditor)
  pls_Editor_Width:Integer;
  // Defines a height of "embedded localization editor" (see EmbeddedEditor)
  pls_Editor_Height:Integer;
  { Defines comma separated list of supported property names for unknown components (ie. Caption, Text, Hint, ...)
    that could be readed by "embedded localization editor" (see EmbeddedEditor) }
  pls_Editor_PropFilter:string;
  // Option to show only predefined components and properties in "embedded localization editor" (see EmbeddedEditor)
  pls_Editor_OnlyPredefinedProps:Boolean;
  { Option to show currently defined text constants in "embedded localization editor" (see EmbeddedEditor).
    Note: if true, then you must not clear the list of constants in custom TLanguageManager.OnLangaugeChanged event. }
  pls_Editor_IncludeConstants:Boolean;
  { This option of "embedded localization editor" defines, that modified and new items will be saved into user language files (.lngu).
    See also EmbeddedEditor procedure. }
  pls_Editor_SaveInUserMode:Boolean;
  { By this option of "embedded localization editor" you can set the default language code for starting localization from scratch ('en' by default).
    See also EmbeddedEditor procedure. }
  pls_Editor_DefaultLangCode:string;
  // Default language name for starting localization from scratch ('English' by default). See also pls_Editor_DefaultLangCode variable and EmbeddedEditor procedure.
  pls_Editor_DefaultLangName:string;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

{$IFDEF FPC}
  {$UNDEF D2009UP}
{$ELSE}
  {$IF CompilerVersion>=20}
    {$DEFINE D2009UP}
  {$ELSE}
    {$UNDEF D2009UP}
  {$IFEND}
{$ENDIF}

uses
  plsDialogs, IniFiles, TypInfo {$IFDEF PLS_LNGZ}, SciZipFile {$ENDIF};

var
  FShortcutRef:TLanguageManager;
  FShortcut:TShortCut;
  FShortcutHook:HHOOK;
  FCaptureRef:TLanguageManager;
  FCaptureHook:HHOOK;
  FCaptureHint:THintWindow;

function _FindPropertyOwnerByPath(const Root:TComponent;PropertyPath:string;var OwnerObject:TObject;var PropertyName:string):Boolean;
var
  p,n:Integer;
  cn:string;
  O,NO:TObject;
  C,NC:TComponent;
  CL:TCollection;
  CLI:TCollectionItem;
begin
  Result:=False;
  try
    p:=Pos('.',PropertyPath);
    if p=0 then
    begin // simple property of root
      OwnerObject:=Root;
      PropertyName:=PropertyPath;
      Result:=True;
    end
    else
    begin
      C:=Root;
      repeat
        cn:=Copy(PropertyPath,1,p-1); // object(component) name
        NC:=C.FindComponent(cn);
        if NC<>nil then
        begin
          C:=NC;
          Delete(PropertyPath,1,p); // rest path
          p:=Pos('.',PropertyPath);
        end;
      until (NC=nil) or (p=0);     // in C is last found component
      if p>0 then // property is encapsulated object in component C
      begin
        O:=C;
        repeat
          cn:=Copy(PropertyPath,1,p-1); // object(component) name
          n:=Pos('[',cn);
          if n>0 then
          begin // collection item
            NO:=nil;
            {$IFDEF FPC}
            CL:=TCollection({$IFDEF Win32}LongInt{$ELSE}Int64{$ENDIF}(GetOrdProp(O,Copy(cn,1,n-1))));
            {$ELSE}
            CL:=TCollection(GetOrdProp(O,Copy(cn,1,n-1)));
            {$ENDIF}
            if CL<>nil then
            begin
              CLI:=CL.FindItemID( strtointdef(Copy(cn,n+1,Pos(']',cn)-n-1),-1) );
              if CLI<>nil then
                NO:=CLI;
            end;
          end
          else
            {$IFDEF FPC}
            NO:=TObject({$IFDEF Win32}LongInt{$ELSE}Int64{$ENDIF}(GetOrdProp(O,cn)));
            {$ELSE}
            NO:=TObject(GetOrdProp(O,cn));
            {$ENDIF}
          if NO<>nil then
          begin
            O:=NO;
            Delete(PropertyPath,1,p); // rest path
            p:=Pos('.',PropertyPath);
          end;
        until (NO=nil) or (p=0);
        if p=0 then
        begin
          OwnerObject:=O;
          PropertyName:=PropertyPath;
          Result:=True;
        end
      end
      else
      begin // simple property of component C
        OwnerObject:=C;
        PropertyName:=PropertyPath;
        Result:=True;
      end;
    end;
  except
    Result:=False;
  end;
end;

{$IFDEF FPC}
{$HINTS OFF}
{$ENDIF}
function _SetPropertyByPath(const Root:TComponent;const PropertyPath:string;Value:Variant; AllowSpecial:Boolean=False):TObject;
var
  GO:TObject;
  PN,tmp:string;
begin
  Result:=nil;
  if _FindPropertyOwnerByPath(Root,PropertyPath,GO,PN) and Assigned(GO) then
  begin
    Result:=GO;
    if Result is TStrings then
    begin
      if AnsiSameText(PN,'CommaText') then
        TStrings(Result).CommaText:=Value
      else
      if AnsiSameText(PN,'DelimitedText') then
        TStrings(Result).DelimitedText:=Value
      else
        TStrings(Result).Text:=Value
    end
    else
    if AllowSpecial then
    begin
      if AnsiSameText(PN,'Shortcut') then
      begin
        tmp:=Value;
        SetOrdProp(GO,PN,TextToShortCut(tmp));
      end
      else
        SetPropValue(GO,PN,Value);
    end
    else
      SetPropValue(GO,PN,Value);
  end;
end;

function _GetShortcutInfo(VK:wParam; Key:lParam): TShortCut;
const
  AltMask = $20000000;
begin
  Result := Byte(VK);
  if Result = 0 then
    Exit;
  if GetKeyState(VK_SHIFT) < 0 then Inc(Result, scShift);
  if GetKeyState(VK_CONTROL) < 0 then Inc(Result, scCtrl);
  if Key and AltMask <> 0 then Inc(Result, scAlt);
end;

function ShortcutHookProc(Code: Integer; VK: WPARAM; Key: LPARAM): LRESULT; stdcall;
begin
  Result:=0;
  if (Code>=0) and Assigned(FShortcutRef) then
  begin
    if (not Boolean(Key shr 31)) and (not Boolean(Key shr 30)) then //key down and no repeated keypress
    begin
      if (VK=VK_ESCAPE) and (FCaptureHook<>0) then
      begin
        Result:=1;
        LE_EndCaptureControl;
      end
      else
      if (_GetShortcutInfo(VK,Key)=FShortcut) then
      begin
        Result:=1;
        if Assigned(FCaptureRef) then
          LE_EndCaptureControl
        else
          LE_BeginCaptureControl(FShortcutRef);
      end;
    end;
  end;
  if Result=0 then
    Result:=CallNextHookEx(FShortcutHook,Code,VK,Key);
end;

procedure LE_InstallCaptureControlShortcut(const LangMan:TLanguageManager; Shortcut:TShortCut);
begin
  FShortcutRef:=LangMan;
  FShortcut:=Shortcut;
  FShortcutHook:=SetWindowsHookEx(WH_KEYBOARD,@ShortcutHookProc,0,GetCurrentThreadID);
end;

procedure LE_RemoveCaptureControlShortcut;
begin
  if FCaptureHook<>0 then
    LE_EndCaptureControl;
  if FShortcutHook <> 0 then
    UnhookWindowsHookEx(FShortcutHook);
  FShortcutHook := 0;
  FShortcutRef:=nil;
  FShortcut:=0;
end;

function FindControlAtPos(Root:TWinControl; const Pos: TPoint ): TControl;
var
  I: Integer;
  LControl: TControl;

  function GetControlAtPos(AControl: TControl): Boolean;
  var
    P:TPoint;
  begin
    with AControl do
    begin
      P := Point(Pos.X - Left, Pos.Y - Top);
      Result := PtInRect(ClientRect, P) and Visible;
    end;
  end;

begin
  Result:=nil;
  for i:=Root.ControlCount-1 downto 0 do
  begin
    LControl:=Root.Controls[i];
    if GetControlAtPos(LControl) then
    begin
      Result:=LControl;
      if LControl is TWinControl then
      begin
        LControl:=FindControlAtPos(TWinControl(LControl),Point(Pos.X - LControl.Left, Pos.Y - LControl.Top));
        if LControl<>nil then
          Result:=LControl;
      end;
      break;
    end;
  end;
end;

function CaptureHookMsgProc(nCode: Integer; wParam: WParam; lParam: LParam): LRESULT; stdcall;
var
  FLM:TLanguageManager;
  P:TPoint;
  R:TRect;
  F:TForm;
  C:TControl;
begin
  Result:=0;
  if nCode>=0 then
  begin
    if FCaptureRef<>nil then
    begin
      if (wParam = WM_LBUTTONDOWN) or (wParam = WM_NCLBUTTONDOWN) then
      begin
        FLM:=FCaptureRef;
        FCaptureRef:=nil;
        F:=Screen.ActiveForm;
        if F<>nil then
        begin
          Result:=1;
          if GetKeyState(VK_SHIFT) < 0 then
            C:=nil
          else
          begin
            GetCursorPos(P);
            P:=F.ScreenToClient(P);
            C:=FindControlAtPos(F,P);
            if C=nil then
              C:=F;
          end;
          Screen.Cursor:=crDefault;
          LE_EndCaptureControl;
          EmbeddedEditor(FLM,F,C,pls_Editor_OnlyPredefinedProps,pls_Editor_IncludeConstants);
        end
        else
          FCaptureRef:=FLM;
      end
      else
      if (wParam = WM_MOUSEMOVE) and Assigned(FCaptureHint) then
      begin
        GetCursorPos(P);
        F:=Screen.ActiveForm;
        if F<>nil then
        begin
          C:=FindControlAtPos(F,F.ScreenToClient(P));
          Inc(P.y,24);
          if Assigned(C) then
            FCaptureHint.Hint := C.Name
          else
            FCaptureHint.Hint := F.ClassName;
          R := FCaptureHint.CalcHintRect(Screen.Width, FCaptureHint.Hint, nil);
          OffsetRect(R, P.x, P.y);
          FCaptureHint.ActivateHint(R,FCaptureHint.Hint);
        end;
      end;
    end;
  end;
  if Result=0 then
    Result := CallNextHookEx(FCaptureHook, nCode, wParam, lParam);
end;

procedure LE_BeginCaptureControl(const LangMan:TLanguageManager);
var
  P:TPoint;
  R:TRect;
  F:TForm;
  C:TControl;
begin
  F:=Screen.ActiveForm;
  if (F<>nil) and (not (F is TfrmEmbeddedEditor)) then
  begin
    if FCaptureHook=0 then
      FCaptureHook:=SetWindowsHookEx(WH_MOUSE,@CaptureHookMsgProc,0,GetCurrentThreadID);
    FCaptureRef:=LangMan;
    Screen.Cursor:=crHandPoint;

    FCaptureHint := THintWindow.Create(Application);
    FCaptureHint.Color := {$IFDEF FPC} $00DEF1FE {$ELSE} Application.HintColor {$ENDIF};
    FCaptureHint.Font := Screen.HintFont;

    GetCursorPos(P);
    C:=FindControlAtPos(F,F.ScreenToClient(P));
    Inc(P.y,24);
    if Assigned(C) then
      FCaptureHint.Hint := C.Name
    else
      FCaptureHint.Hint := F.ClassName;
    R := FCaptureHint.CalcHintRect(Screen.Width, FCaptureHint.Hint, nil);
    OffsetRect(R, P.x, P.y);
    FCaptureHint.ActivateHint(R,FCaptureHint.Hint);
  end;
end;

procedure LE_EndCaptureControl;
begin
  if FCaptureHook <> 0 then
    UnhookWindowsHookEx(FCaptureHook);
  FCaptureHook := 0;
  if Assigned(FCaptureHint) then
  begin
    FCaptureHint.ReleaseHandle;
    FCaptureHint.Free;
    FCaptureHint:=nil;
  end;
  FCaptureRef:=nil;
  Screen.Cursor:=crDefault;
end;

procedure EmbeddedEditor(const LangMan:TLanguageManager; const Owner:TComponent; const OnlyComponent:TComponent=nil;
  OnlyPredefinedProps:Boolean=False; IncludeConstants:Boolean=False; Title:string='');
var
  F:TfrmEmbeddedEditor;
  P,P2:TPoint;
  TBR:TRect;
  Msg: TMsg;
begin
  if Assigned(LangMan) and Assigned(Owner) then
  begin
    // create an Embedded Editor form
    F:=TfrmEmbeddedEditor.Create(Application);
    try
      // set captions
      if Length(Title)>0 then
        F.Caption:=Title
      else
      if Length(pls_Editor_Title)>0 then
        F.Caption:=pls_Editor_Title
      else
        F.Caption:=Application.Title;
      F.sbOK.Caption:=lng_ButtonCaptions[mbOK];
      F.sbCancel.Caption:=lng_ButtonCaptions[mbCancel];

      // set position on screen
      GetCursorPos(P);
      if Assigned(OnlyComponent) and (OnlyComponent is TControl) then
      begin
        P2:=TControl(OnlyComponent).ClientToScreen(point(0,TControl(OnlyComponent).Height+2));
        if (P2.y-P.y>0) and (P2.y-P.y<32) then
          P:=P2;
      end;
      TBR:=F.Monitor.WorkareaRect;
      if p.x+pls_Editor_Width>TBR.Right then
        p.x:=TBR.Right-pls_Editor_Width;
      if p.y+pls_Editor_Height>TBR.Bottom then
        p.y:=TBR.Bottom-pls_Editor_Height;
      F.SetBounds(P.x,P.y,pls_Editor_Width,pls_Editor_Height);
      Application.CancelHint;

      // prepare items to localize
      F.FLangMan:=LangMan;
      F.FRoot:=Owner;
      F.FOnlyComponent:=OnlyComponent;
      F.PrepareItems(OnlyPredefinedProps, IncludeConstants);
      F.FModified:=False;

      // remove mouse messages from queue, to prevent window flickering
      while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE or PM_NOYIELD) do;

      // display the editor
      F.ShowModal;
    finally
      F.Release;
    end;
  end;
end;

{$IFDEF FPC}
{$HINTS ON}
{$ENDIF}

procedure TfrmEmbeddedEditor.PrepareItems(OnlyPredefinedProps:Boolean=False; IncludeConstants:Boolean=False);
var
  firstrow:Boolean;
  j,f:Integer;
  mwidth:Integer;
  CL:TStringList;
  LProps:TPropList;
  fC:TComponent;

  function _Capitalize(const id:string):string;
  var
    i:Integer;
  begin
    result:=lowercase(id);
    if Length(Result)>0 then
    begin
      Result[1]:=UpCase(Result[1]);
      for i:=2 To Length(Result)-1 do
        if Result[i]='.' then
          Result[i+1]:=UpCase(Result[i+1]);
    end;
  end;

  procedure _AddText(const ID, Value:string; AsNew:boolean; Comp:TComponent);
  var
    w:Integer;
  begin
    if firstrow then firstrow:=False else sgItems.RowCount:=sgItems.RowCount+1;
    sgItems.Cells[0,sgItems.RowCount-1]:='';
    sgItems.Cells[1,sgItems.RowCount-1]:=ID;
    sgItems.Cells[2,sgItems.RowCount-1]:=Value;
    if AsNew then
      sgItems.Objects[0,sgItems.RowCount-1]:={$IFDEF FPC} TObject(Pointer(1)) {$ELSE} Pointer(1) {$ENDIF} ; // item edit state: 0-none, 1-new (not in language yet), 2-updated, 3-inserted, 4-deleted
    sgItems.Objects[2,sgItems.RowCount-1]:=Comp; // component reference
    w:=sgItems.Canvas.TextWidth(ID);
    if w>mwidth then
      mwidth:=w;
  end;

  procedure _AddProp(idx:Integer; const ID:string; Comp:TComponent);
  begin
    _AddText(ID,PChar(Pointer(FLangMan.Properties.Objects[idx])^),False,Comp)
  end;

  procedure _AddConst(idx:Integer);
  begin
    _AddText(FLangMan.Constants[idx],PChar(Pointer(FLangMan.Constants.Objects[idx])^),False,nil);
    sgItems.Objects[1,sgItems.RowCount-1]:={$IFDEF FPC} TObject(Pointer(1)) {$ELSE} Pointer(1) {$ENDIF}; // it is a constant
  end;

  procedure _AddComponent(Comp:TComponent);
  var
    cname,rname:string;
    i,s,c,cc,k:Integer;
    fnd:Boolean;
    O:TObject;
    CProps:TPropList;
    tmp:string;
    tmpProps:string;
  begin
    if Comp=FRoot then
    begin
      cname:='Self';
      rname:=UpperCase(FRoot.ClassName+'.'+cname+'.');
    end
    else
    begin
      if Length(Comp.Name)=0 then
        Exit;
      cname:=Comp.Name;
      if Assigned(Comp.Owner) and (Comp.Owner is TFrame) then
        rname:=UpperCase(Comp.Owner.ClassName+'.'+Comp.Name+'.')
      else
        rname:=UpperCase(FRoot.ClassName+'.'+Comp.Name+'.');
    end;
    c:=Length(rname);
    tmpProps:='';
    fnd:=False;
    FLangMan.Properties.Find(rname,s);
    if s<0 then s:=0;
    for i:=s To FLangMan.Properties.Count-1 do
      if Copy(FLangMan.Properties[i],1,c)=rname then
      begin
        tmp:=Copy(FLangMan.Properties[i],c+1,maxint);
        _AddProp(i,cname+'.'+_Capitalize(tmp),Comp);
        tmpProps:=tmpProps+lowercase(tmp)+',';
        if not fnd then fnd:=True;
      end
      else
        break;
    if (not fnd) or (not OnlyPredefinedProps) then
    begin
      if (Comp is TControl) and (TControl(Comp).Action<>nil) then
      begin
        if (TControl(Comp).Action.Owner=FRoot) or (TControl(Comp).Action.Owner is TFrame) then
          _AddComponent(TControl(Comp).Action)
      end
      else
      if not OnlyPredefinedProps then
      begin
        c:=GetPropList(Comp.ClassInfo,[tkLString, tkString, {$IFDEF FPC} tkAString, tkUString, {$ELSE} {$IF CompilerVersion>=20} tkUString, {$IFEND} {$ENDIF} tkWString, tkClass], @LProps, True);
        rname:=UpperCase(pls_Editor_PropFilter+',');
        for i:=0 To c-1 do
        begin
          if LProps[i]^.PropType^.Kind=tkClass then
          begin
            if GetTypeData(LProps[i]^.PropType{$IFNDEF FPC}^{$ENDIF})^.ClassType.InheritsFrom(TStrings) then
            begin
              {$IFDEF FPC}
              O:=TObject({$IFDEF Win32}LongInt{$ELSE}Int64{$ENDIF}(GetOrdProp(Comp,LProps[i])));
              {$ELSE}
              O:=TObject(GetOrdProp(Comp,LProps[i]));
              {$ENDIF}
              {$IFDEF D2009UP}
              if (O<>nil) and ((not fnd) or (Pos(lowercase(string(LProps[i].Name)+'.Text')+',',tmpProps)=0)) then
                _AddText(cname+'.'+string(LProps[i].Name)+'.Text',TStrings(O).Text,True,Comp);
              {$ELSE}
              if (O<>nil) and ((not fnd) or (Pos(lowercase(LProps[i]^.Name+'.Text')+',',tmpProps)=0)) then
                _AddText(cname+'.'+LProps[i]^.Name+'.Text',TStrings(O).Text,True,Comp);
              {$ENDIF}
            end
            else
            if GetTypeData(LProps[i]^.PropType{$IFNDEF FPC}^{$ENDIF})^.ClassType.InheritsFrom(TCollection) then
            begin
              {$IFDEF FPC}
              O:=TObject({$IFDEF Win32}LongInt{$ELSE}Int64{$ENDIF}(GetOrdProp(Comp,LProps[i])));
              {$ELSE}
              O:=TObject(GetOrdProp(Comp,LProps[i]));
              {$ENDIF}
              if (O<>nil) and (TCollection(O).Count>0) then
              begin
                cc:=GetPropList(TCollection(O).Items[0].ClassInfo,[tkLString, tkString, {$IFDEF FPC} tkAString, tkUString, {$ELSE} {$IF CompilerVersion>=20} tkUString, {$IFEND} {$ENDIF} tkWString], @CProps, True);                
                for s:=0 To TCollection(O).Count-1 do
                  for k:=0 To cc-1 do
                    {$IFDEF D2009UP}
                    if (Pos(UpperCase(string(CProps[k].Name))+',',rname)>0)
                      and ((not fnd) or (Pos(lowercase(string(LProps[i].Name)+'['+IntToStr(s)+'].'+string(CProps[k].Name))+',',tmpProps)=0)) then
                      _AddText(cname+'.'+string(LProps[i].Name)+'['+IntToStr(s)+'].'+string(CProps[k].Name),GetPropValue(TCollection(O).Items[s],string(CProps[k].Name)),True,Comp);
                    {$ELSE}
                    if (Pos(UpperCase(CProps[k]^.Name)+',',rname)>0)
                      and ((not fnd) or (Pos(lowercase(LProps[i]^.Name+'['+IntToStr(s)+'].'+CProps[k]^.Name)+',',tmpProps)=0)) then
                      _AddText(cname+'.'+LProps[i]^.Name+'['+IntToStr(s)+'].'+CProps[k]^.Name,GetPropValue(TCollection(O).Items[s],CProps[k]^.Name),True,Comp);
                    {$ENDIF}
              end;
            end;
          end
          else
          {$IFDEF D2009UP}
          if (Pos(UpperCase(string(LProps[i].Name))+',',rname)>0) and ((not fnd) or (Pos(lowercase(string(LProps[i].Name))+',',tmpProps)=0)) then
          begin
            if FLangMan.AllowSpecialProps then
            begin
              if (LProps[i].PropType^.Kind=tkInteger) and (LProps[i].PropType^.Name='ShortCut') then
                _AddText(cname+'.'+string(LProps[i].Name), ShortCutToText(getordprop(Comp,LProps[i])), True, Comp)
              else
                _AddText(cname+'.'+string(LProps[i].Name), GetPropValue(Comp, LProps[i]), True, Comp);
            end
            else
              _AddText(cname+'.'+string(LProps[i].Name), GetPropValue(Comp, LProps[i]), True, Comp);
          end;
          {$ELSE}
          if (Pos(UpperCase(LProps[i]^.Name)+',',rname)>0) and ((not fnd) or (Pos(lowercase(LProps[i]^.Name)+',',tmpProps)=0)) then
          begin
            if FLangMan.AllowSpecialProps then
            begin
              if (LProps[i]^.PropType^.Kind=tkInteger) and (LProps[i]^.PropType^.Name='ShortCut') then
                _AddText(cname+'.'+LProps[i]^.Name, ShortCutToText(getordprop(Comp,LProps[i])), True, Comp)
              else
                _AddText(cname+'.'+LProps[i]^.Name, GetPropValue(Comp, LProps[i]^.Name), True, Comp);
            end
            else
              _AddText(cname+'.'+LProps[i]^.Name, GetPropValue(Comp, LProps[i]^.Name), True, Comp);
          end;
          {$ENDIF}
        end;
      end;
    end;
  end;

begin
  firstrow:=True;
  mwidth:=0;
  sgItems.ColCount:=3;
  sgItems.RowCount:=2;
  sgItems.Cells[0,0]:=''; sgItems.Cells[1,0]:='ID';
  if FLangMan.LanguageCode='' then
    sgItems.Cells[2,0]:=pls_Editor_DefaultLangName+' ('+pls_Editor_DefaultLangCode+')'
  else
    sgItems.Cells[2,0]:=FLangMan.LanguageName+' ('+FLangMan.LanguageCode+')';
  sgItems.Cells[0,1]:=''; sgItems.Cells[1,1]:=''; sgItems.Cells[2,1]:=''; sgItems.Objects[0,1]:=nil; sgItems.Objects[1,1]:=nil; sgItems.Objects[2,1]:=nil;
  if not Assigned(FRoot) then
    Exit;

  if Assigned(FOnlyComponent) then
    _AddComponent(FOnlyComponent)
  else
  begin
    CL:=TStringList.create;
    try
      CL.AddObject('Self',FRoot);
      for j:=0 To FRoot.ComponentCount-1 do
      begin
        fC:=FRoot.Components[j];
        CL.AddObject(fC.Name,fC);
        if fC is TFrame then
          for f:=0 to fC.ComponentCount-1 do
            CL.AddObject(fC.Components[f].Name,fC.Components[f]);
      end;
      CL.Sort;
      for j:=0 To CL.Count-1 do
        _AddComponent(TComponent(CL.Objects[j]))
    finally
      CL.Free;
    end;
  end;
  if IncludeConstants then
    for j:=0 To FLangMan.Constants.Count-1 do
      _AddConst(j);
  sgItems.ColWidths[0]:=17;
  if mwidth>0 then
    sgItems.ColWidths[1]:=mwidth+8;
  sgItems.ColWidths[2]:= sgItems.Width-sgItems.ColWidths[1]-sgItems.ColWidths[0] {$IFDEF FPC} - 4 {$ENDIF};
  sgItems.Col:=2;
  sgItems.Row:=1;
end;

{$IFDEF FPC}
{$WARNINGS OFF}
{$ENDIF}

// Saves language items into appropriate language files or creates a new files (When UserMode=true, only user files - .lngu - are used)
procedure TfrmEmbeddedEditor.SaveItems;
{$IFNDEF D2009UP}
const
 utf8preamb:string = #$EF#$BB#$BF;
{$ENDIF}
var
  i,j,p:Integer;
  UF,PF,            // User lngu files, Product lng files
  ZF,               // Product compressed lngz files
  MF:TStringList;   // list of modified language files
  tmp:string;
  tmpBool:Boolean;
  Ini,PropIni:TMemIniFile;
  PropIniFileName:string;
  tmpIniFileName:string;
  fRootClassName:string;
  Comp:TComponent;
  fLangConsts:TStringList;
  fLangProps:TStringList;
  fLanguageCode:string;
  fLanguageName:string;
  fldr:string;
  {$IFDEF PLS_LNGZ}
  Zip:TZipFile;
  MS:TMemoryStream;
  d:AnsiString;
  ZS:TStringList;
  {$ELSE}
  {$IFNDEF D2009UP}
  ZS:TStringList;
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF D2009UP}
  FS:TFileStream;
  {$ENDIF}

  procedure _UpdateInLangMan(const Lst:TStringList; ID:string; const Value:string);
  var
  	n:^string;
    k:Integer;
  begin
    ID:=AnsiUpperCase(ID);
    if Lst.Find(ID,k) then
    begin
      n:=Pointer(Lst.Objects[k]);
      n^:=Value;
    end
    else
    begin
      new(n);
      n^:=Value;
      Lst.InsertObject(k,ID,{$IFDEF FPC} TObject(Pointer(n)) {$ELSE} Pointer(n) {$ENDIF});
    end;
  end;

  procedure _RemoveFromLangMan(const Lst:TStringList; ID:string);
  var
    k:Integer;
  	n:^string;
  begin
    if Lst.Find(AnsiUpperCase(ID),k) then
    begin
      n:=Pointer(Lst.Objects[k]);
      Dispose(n);
      Lst.Delete(k);
    end;
  end;

begin
  if FModified then
  begin
    try
      // prepare
      fLangConsts:=FLangMan.Constants;
      fLangProps:=FLangMan.Properties;
      fLanguageCode:=FLangMan.LanguageCode;
      fLanguageName:=FLangMan.LanguageName;
      if fLanguageCode='' then
      begin
        fLanguageCode:=pls_Editor_DefaultLangCode;
        fLanguageName:=pls_Editor_DefaultLangName;
      end;

      PF:=TStringList.Create;
      UF:=TStringList.Create;
      MF:=TStringList.Create;
      ZF:=TStringList.Create;
      fLangConsts.Sorted:=False;
      fLangProps.Sorted:=False;
      try
        // get all product (PF) and user (UF) langauge filenames for the product in language folder
        FLangMan.GetLanguageFiles(fLanguageCode,PF,UF);

        // extend the list with lngz files
        for j:=PF.Count-1 downto 0 do
          if lowercase(ExtractFileExt(PF[j]))='.'+LANGFILEEXTZ then
          begin
            {$IFDEF PLS_LNGZ}
            Zip:=TZipFile.Create;
            MS:=TMemoryStream.Create;
            ZS:=TStringList.Create;
            try
              Zip.LoadFromFile(PF[j]);
              for i:=0 To Zip.Count-1 do
              begin
                Ini:=TMemIniFile.Create('');
                {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                d:=Zip.Data[i];
                MS.Clear;
                ZS.Clear;
                {$IFDEF D2009UP}
                MS.Write(d[1],Length(d));
                MS.Position:=0;
                ZS.LoadFromStream(MS);
                {$ELSE}
                {$IFDEF FPC}
                ZS.Text:=d;
                {$ELSE}
                if Copy(d,1,3)=#$EF#$BB#$BF then
                  Delete(d,1,3);
                ZS.Text:=UTF8Decode(d);
                {$ENDIF}
                {$ENDIF}
                Ini.SetStrings(ZS);
                PF.AddObject('$'+PF[j]+'$'+Zip.Name[i],Ini);
              end;
            finally
              ZS.Free;
              MS.Free;
              ZF.AddObject(PF[j],Zip);
            end;
            {$ENDIF}
            PF.Delete(j);
          end;

        // add new lng/lngu files if no one found
        if PF.Count+UF.Count=0 then
        begin
          fldr:=IncludeTrailingPathDelimiter(FLangMan.Folder);
          if not ForceDirectories(fldr) then
            fldr:=ExtractFilePath(ParamStr(0));
          PF.Add(fldr+ChangeFileExt(ExtractFileName(ParamStr(0)),'.'+fLanguageCode+'.'+LANGFILEEXT));
          UF.Add(fldr+ChangeFileExt(ExtractFileName(ParamStr(0)),'.'+fLanguageCode+'.'+LANGFILEEXTU));
        end
        else
        if UF.Count=0 then
          UF.Add(ChangeFileExt(PF[0],'.'+LANGFILEEXTU))
        else
        if PF.Count=0 then
          PF.Add(ChangeFileExt(UF[0],'.'+LANGFILEEXT));

        if pls_Editor_SaveInUserMode then
        begin
          // save items in UserMode (into the .lngu file)
          for j:=1 To sgItems.RowCount-1 do
            if Cardinal(sgItems.Objects[0,j]) in [2,3,4] then
            begin
              Ini:=nil;
              if Cardinal(sgItems.Objects[1,j])=1 then
              begin
                // save constant item
                tmpBool:=False;
                for i:=UF.Count-1 downto 0 do
                begin
                  if UF.Objects[i]=nil then
                  begin
                    Ini:=_CreateLngIni(UF[i]);
                    {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                    {$IFDEF D2009UP}
                    if not FileExists(UF[i]) then
                      Ini.Encoding:=TEncoding.UTF8;
                    {$ENDIF}
                    UF.Objects[i]:=Ini;
                  end
                  else
                    Ini:=TMemIniFile(UF.Objects[i]);
                  if Ini.ValueExists(plsLangMan.lini_sec_consts,sgItems.Cells[1,j]) then
                  begin
                    tmpBool:=True;
                    break;
                  end;
                end;
                if not tmpBool then
                begin
                  tmp:='';
                  for i:=PF.Count-1 downto 0 do
                  begin
                    if PF.Objects[i]=nil then
                    begin
                      Ini:=_CreateLngIni(PF[i]);
                      {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                      {$IFDEF D2009UP}
                      if not FileExists(PF[i]) then
                        Ini.Encoding:=TEncoding.UTF8;
                      {$ENDIF}
                      PF.Objects[i]:=Ini;
                    end
                    else
                      Ini:=TMemIniFile(PF.Objects[i]);
                    if Ini.ValueExists(plsLangMan.lini_sec_consts,sgItems.Cells[1,j]) or (i=0) then
                    begin
                      tmp:=changefileext(PF[i],'.'+LANGFILEEXTU);
                      break;
                    end;
                  end;
                  if tmp='' then
                    Ini:=nil
                  else
                  begin
                    p:=-1;
                    for i:=0 To UF.Count-1 do
                      if AnsiCompareFileName(tmp,UF[i])=0 then
                      begin
                        p:=i;
                        break;
                      end;
                    if p=-1 then
                      p:=UF.Add(tmp);
                    if UF.Objects[p]=nil then
                    begin
                      Ini:=_CreateLngIni(UF[p]);
                      {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                      {$IFDEF D2009UP}
                      if not FileExists(UF[p]) then
                        Ini.Encoding:=TEncoding.UTF8;
                      {$ENDIF}
                      UF.Objects[p]:=Ini;
                    end
                    else
                      Ini:=TMemIniFile(UF.Objects[p]);
                  end;
                end;
                if Assigned(Ini) then
                begin
                  case Cardinal(sgItems.Objects[0,j]) of
                    2,3:  // newly added or modified
                      begin
                        _UpdateInLangMan(fLangConsts,sgItems.Cells[1,j],sgItems.Cells[2,j]);
                        Ini.WriteString(plsLangMan.lini_sec_consts,sgItems.Cells[1,j],plsLangMan._ECodeText(sgItems.Cells[2,j])); // text must be stored as _ecoded (_ECodeText)
                      end;
                    4:begin // deleted
                        Ini.DeleteKey(plsLangMan.lini_sec_consts,sgItems.Cells[1,j]);
                      end;
                  end;
                  if MF.IndexOfObject(Ini)=-1 then
                    MF.AddObject(Ini.FileName,Ini);  // mark the Ini file as modified
                end;
              end
              else
              begin
                // save property item
                if sgItems.Objects[2,j]<>nil then
                begin
                  Comp:=TComponent(sgItems.Objects[2,j]);
                  if Comp.Owner is TFrame then
                    fRootClassName:=Comp.Owner.ClassName
                  else
                    fRootClassName:=FRoot.ClassName;
                end
                else
                  fRootClassName:=FRoot.ClassName;
                tmpBool:=False;
                // find property (or an appropriate section) in lngu files
                for i:=UF.Count-1 downto 0 do
                begin
                  if UF.Objects[i]=nil then
                  begin
                    Ini:=_CreateLngIni(UF[i]);
                    {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                    {$IFDEF D2009UP}
                    if not FileExists(UF[i]) then
                      Ini.Encoding:=TEncoding.UTF8;
                    {$ENDIF}
                    UF.Objects[i]:=Ini;
                  end
                  else
                    Ini:=TMemIniFile(UF.Objects[i]);
                  if Ini.ValueExists(fRootClassName,sgItems.Cells[1,j]) then
                  begin
                    tmpBool:=True;
                    break;
                  end
                end;
                if not tmpBool then
                begin
                  // find property (or section) in lng files and open an appropriate lngu file
                  tmp:='';
                  for i:=PF.Count-1 downto 0 do
                  begin
                    if PF.Objects[i]=nil then
                    begin
                      Ini:=_CreateLngIni(PF[i]);
                      {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                      {$IFDEF D2009UP}
                      if not FileExists(PF[i]) then
                        Ini.Encoding:=TEncoding.UTF8;
                      {$ENDIF}
                      PF.Objects[i]:=Ini;
                    end
                    else
                      Ini:=TMemIniFile(PF.Objects[i]);
                    if Ini.ValueExists(fRootClassName,sgItems.Cells[1,j]) then
                    begin
                      tmp:=changefileext(PF[i],'.'+LANGFILEEXTU);
                      break;
                    end
                    else
                    if Ini.SectionExists(fRootClassName) or (i=0) then
                      tmp:=changefileext(PF[i],'.'+LANGFILEEXTU);
                  end;
                  if tmp='' then
                    Ini:=nil
                  else
                  begin
                    p:=-1;
                    for i:=0 To UF.Count-1 do
                      if AnsiCompareFileName(tmp,UF[i])=0 then
                      begin
                        p:=i;
                        break;
                      end;
                    if p=-1 then
                      p:=UF.Add(tmp);
                    if UF.Objects[p]=nil then
                    begin
                      Ini:=_CreateLngIni(UF[p]);
                      {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                      {$IFDEF D2009UP}
                      if not FileExists(UF[p]) then
                        Ini.Encoding:=TEncoding.UTF8;
                      {$ENDIF}
                      UF.Objects[p]:=Ini;
                    end
                    else
                      Ini:=TMemIniFile(UF.Objects[p]);
                  end;
                end;
                if Assigned(Ini) then
                begin
                  case Cardinal(sgItems.Objects[0,j]) of
                    2,3:  // newly added or modified
                      begin
                        _UpdateInLangMan(fLangProps,fRootClassName+'.'+sgItems.Cells[1,j],sgItems.Cells[2,j]);
                        Ini.WriteString(fRootClassName,sgItems.Cells[1,j],plsLangMan._ECodeText(sgItems.Cells[2,j])); // text must be stored as _ecoded (_ECodeText)
                      end;
                    4:begin // deleted
                        Ini.DeleteKey(fRootClassName,sgItems.Cells[1,j]);
                      end;
                  end;
                  if MF.IndexOfObject(Ini)=-1 then
                    MF.AddObject(Ini.FileName,Ini);  // mark the Ini file as modified
                end;
              end;
            end;
        end
        else
        begin
          // save items in lng/lngz files
          PropIni:=nil;
          for j:=1 To sgItems.RowCount-1 do
            if Cardinal(sgItems.Objects[0,j]) in [2,3,4] then
            begin
              Ini:=nil;
              if Cardinal(sgItems.Objects[1,j])=1 then
              begin
                // save constant item
                for i:=PF.Count-1 downto 0 do
                begin
                  if PF.Objects[i]=nil then
                  begin
                    Ini:=_CreateLngIni(PF[i]);
                    {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                    {$IFDEF D2009UP}
                    if not FileExists(PF[i]) then
                      Ini.Encoding:=TEncoding.UTF8;
                    {$ENDIF}
                    PF.Objects[i]:=Ini;
                  end
                  else
                    Ini:=TMemIniFile(PF.Objects[i]);
                  if Ini.ValueExists(plsLangMan.lini_sec_consts,sgItems.Cells[1,j]) or (i=0) then
                  begin
                    case Cardinal(sgItems.Objects[0,j]) of
                      2,3:  // newly added or modified
                        begin
                          _UpdateInLangMan(fLangConsts,sgItems.Cells[1,j],sgItems.Cells[2,j]);
                          Ini.WriteString(plsLangMan.lini_sec_consts,sgItems.Cells[1,j],plsLangMan._ECodeText(sgItems.Cells[2,j])); // text must be stored as _ecoded (_ECodeText)
                        end;
                      4:begin // deleted
                          _RemoveFromLangMan(fLangConsts,sgItems.Cells[1,j]);
                          Ini.DeleteKey(plsLangMan.lini_sec_consts,sgItems.Cells[1,j]);
                        end;
                    end;
                    if MF.IndexOfObject(Ini)=-1 then
                    begin
                      // mark the Ini file as modified
                      if Length(Ini.FileName)>0 then
                        MF.AddObject(Ini.FileName,Ini)
                      else
                        MF.AddObject(PF[i],Ini)
                    end;
                    break;
                  end;
                end;
              end
              else
              begin
                // save property item
                if sgItems.Objects[2,j]<>nil then
                begin
                  Comp:=TComponent(sgItems.Objects[2,j]);
                  if Comp.Owner is TFrame then
                    fRootClassName:=Comp.Owner.ClassName
                  else
                    fRootClassName:=FRoot.ClassName;
                end
                else
                  fRootClassName:=FRoot.ClassName;
                tmpBool:=False;
                for i:=PF.Count-1 downto 0 do
                begin
                  if PF.Objects[i]=nil then
                  begin
                    Ini:=_CreateLngIni(PF[i]);
                    {$IFDEF FPC} Ini.CacheUpdates:=False; {$ENDIF}
                    {$IFDEF D2009UP}
                    if not FileExists(PF[i]) then
                      Ini.Encoding:=TEncoding.UTF8;
                    {$ENDIF}
                    PF.Objects[i]:=Ini;
                  end
                  else
                    Ini:=TMemIniFile(PF.Objects[i]);
                  tmpIniFileName:=PF[i];
                  if Ini.ValueExists(fRootClassName,sgItems.Cells[1,j]) then
                  begin
                    PropIni:=Ini;
                    PropIniFileName:=PF[i];
                    tmpBool:=True;
                    break;
                  end
                  else
                  if (PropIni=nil) and (Ini.SectionExists(fRootClassName) or (i=0)) then
                  begin
                    PropIni:=Ini;
                    PropIniFileName:=PF[i];
                  end;
                end;
                if not tmpBool then
                begin
                  Ini:=PropIni;
                  tmpIniFileName:=PropIniFileName;
                end;
                if Assigned(Ini) then
                begin
                  case Cardinal(sgItems.Objects[0,j]) of
                    2,3:  // newly added or modified
                      begin
                        _UpdateInLangMan(fLangProps,fRootClassName+'.'+sgItems.Cells[1,j],sgItems.Cells[2,j]);
                        Ini.WriteString(fRootClassName,sgItems.Cells[1,j],plsLangMan._ECodeText(sgItems.Cells[2,j])); // text must be stored as _ecoded (_ECodeText)
                      end;
                    4:begin // deleted
                        _RemoveFromLangMan(fLangProps,fRootClassName+'.'+sgItems.Cells[1,j]);
                        Ini.DeleteKey(fRootClassName,sgItems.Cells[1,j]);
                      end;
                  end;
                  if MF.IndexOfObject(Ini)=-1 then
                  begin
                    // mark the Ini file as modified
                    if Length(Ini.FileName)>0 then
                      MF.AddObject(Ini.FileName,Ini)
                    else
                      MF.AddObject(tmpIniFileName,Ini)
                  end
                end;
              end;
            end;
        end;

        // save and destroy Ini objects
        for i:=0 To PF.Count-1 do
          if PF.Objects[i]<>nil then
          begin
            Ini:=TMemIniFile(PF.Objects[i]);
            if MF.IndexOfObject(Ini)>=0 then
            begin
              tmpIniFileName:=PF[i];
              j:=-1;
              if tmpIniFileName[1]='$' then
              begin
                Delete(tmpIniFileName,1,1);
                p:=Pos('$',tmpIniFileName);
                if p>0 then
                begin
                  j:=ZF.IndexOf(Copy(tmpIniFileName,1,p-1));
                  Delete(tmpIniFileName,1,p);
                end;
              end;
              if (j>=0) and (Length(tmpIniFileName)>0) then
              begin
                {$IFDEF PLS_LNGZ}
                Zip:=TZipFile(ZF.Objects[j]);
                MS:=TMemoryStream.Create;
                ZS:=TStringList.Create;
                try
                  Ini.GetStrings(ZS);
                  {$IFDEF D2009UP}
                  ZS.SaveToStream(MS,TEncoding.UTF8);
                  {$ELSE}
                  {$IFDEF FPC}
                  MS.WriteBuffer(Pointer(utf8preamb)^,Length(utf8preamb));
                  ZS.SaveToStream(MS);
                  {$ELSE}
                  d:=utf8preamb+utf8encode(ZS.Text);
                  {$ENDIF}
                  {$ENDIF}
                  {$IF Defined(D2009UP) OR Defined(FPC)}
                  MS.Position:=0;
                  SetLength(d,MS.Size);
                  MS.Read(d[1],MS.size);
                  {$IFEND}

                  p:=Zip.IndexOfFile(tmpIniFileName);
                  if p>=0 then
                  begin
                    Zip.Files[p].CommonFileHeader.LastModFileTimeDate := DateTimeToFileDate(now);
                    Zip.Data[p]:=d;
                  end;
                  if MF.IndexOfObject(Zip)=-1 then
                    MF.AddObject(ZF[j],Zip);
                finally
                  MS.Free;
                  ZS.Free;
                end;
                {$ENDIF}
              end
              else
              begin
                if not Ini.SectionExists(lini_sec_main) then
                  Ini.WriteString(lini_sec_main,lini_key_name,fLanguageName);
                {$IFDEF D2009UP}
                Ini.UpdateFile;
                {$ELSE}
                FS:=TFileStream.Create(Ini.FileName,fmCreate);
                ZS:=TStringList.Create;
                try
                  Ini.GetStrings(ZS);
                  FS.WriteBuffer(Pointer(utf8preamb)^,Length(utf8preamb));
                  {$IFNDEF FPC}
                  ZS.Text:=UTF8Encode(ZS.Text);
                  {$ENDIF}
                  ZS.SaveToStream(FS);
                finally
                  FS.Free;
                  ZS.Free;
                end;
                {$ENDIF}
              end;
            end;
            Ini.Free;
            PF.Objects[i]:=nil;
          end;
        for i:=0 To UF.Count-1 do
          if UF.Objects[i]<>nil then
          begin
            Ini:=TMemIniFile(UF.Objects[i]);
            if MF.IndexOfObject(Ini)>=0 then
            begin
              if not Ini.SectionExists(lini_sec_main) then
                Ini.WriteString(lini_sec_main,lini_key_name,fLanguageName);
              {$IFDEF D2009UP}
              Ini.UpdateFile;
              {$ELSE}
              FS:=TFileStream.Create(Ini.FileName,fmCreate);
              ZS:=TStringList.Create;
              try
                Ini.GetStrings(ZS);
                FS.WriteBuffer(Pointer(utf8preamb)^,Length(utf8preamb));
                {$IFNDEF FPC}
                ZS.Text:=UTF8Encode(ZS.Text);
                {$ENDIF}
                ZS.SaveToStream(FS);
              finally
                FS.Free;
                ZS.Free;
              end;
              {$ENDIF}
            end;
            Ini.Free;
            UF.Objects[i]:=nil;
          end;

        for i:=0 To ZF.Count-1 do
          if ZF.Objects[i]<>nil then
          begin
            {$IFDEF PLS_LNGZ}
            Zip:=TZipFile(ZF.Objects[i]);
            if MF.IndexOfObject(Zip)>=0 then
              Zip.SaveToFile(ZF[i]);
            Zip.Free;
            {$ENDIF}
            ZF.Objects[i]:=nil;
          end;

      finally
        fLangConsts.Sorted:=True;
        fLangProps.Sorted:=True;
        ZF.Free;
        MF.Free;
        UF.Free;
        PF.Free;
      end;
    except
      on E:Exception do
        MessageDlgLM(Caption,e.message,mtWarning)
    end;
  end;
end;

procedure TfrmEmbeddedEditor.FormResize(Sender: TObject);
begin
  sgItems.ColWidths[2]:=sgItems.Width-sgItems.ColWidths[1]-sgItems.ColWidths[0];
end;

procedure TfrmEmbeddedEditor.sgItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgItems.Canvas.Font := sgItems.Font;
  if (gdSelected in State) and (not (gdFocused in State) or
    ([goDrawFocusSelected, goRowSelect] * sgItems.Options <> [])) then
  begin
    sgItems.Canvas.Brush.Color := clHighlight;
    sgItems.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    if ARow<sgItems.FixedRows then
      sgItems.Canvas.Brush.Color := sgItems.FixedColor
    else
    begin
      if ACol=2 then
        sgItems.Canvas.Brush.Color := sgItems.Color
      else
        sgItems.Canvas.Brush.Color := clBtnHighlight;
      if Cardinal(sgItems.Objects[0,ARow])=1 then // default by component (not in language yet)
        sgItems.Canvas.Font.Color := clGrayText;
    end;
  end;
  sgItems.Canvas.FillRect(Rect);
  if (ACol=0) and (ARow>=sgItems.FixedRows) and (Cardinal(sgItems.Objects[0,ARow]) in [2,3,4]) then
  begin
    case Cardinal(sgItems.Objects[0,ARow]) of
      2:sgItems.Canvas.Brush.Color:=$00C89090;  // updated
      3:sgItems.Canvas.Brush.Color:=$0090C890;  // inserted
      4:sgItems.Canvas.Brush.Color:=$009090C8;  // deleted
    end;
    sgItems.Canvas.FillRect(Classes.Rect(Rect.Left+4,Rect.Top+5,Rect.Right-4,Rect.Bottom-5));
  end;
  if Length(sgItems.Cells[ACol, ARow])>0 then
    sgItems.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, sgItems.Cells[ACol, ARow]);
  if ARow<sgItems.FixedRows then
    sgItems.Canvas.Pen.Color:=clGray
  else
    sgItems.Canvas.Pen.Color:=clSilver;
  sgItems.Canvas.Polyline([point(Rect.Left,Rect.Bottom {$IFDEF FPC}-1{$ENDIF}),point(Rect.Right,Rect.Bottom{$IFDEF FPC}-1{$ENDIF})]);
  if ACol<sgItems.ColCount-1 then
    sgItems.Canvas.Polyline([point(Rect.Right{$IFDEF FPC}-1{$ENDIF},Rect.Top),point(Rect.Right{$IFDEF FPC}-1{$ENDIF},Rect.Bottom)]);
end;

type
  TGridWraper = class(TCustomGrid);

procedure TfrmEmbeddedEditor.sgItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
  begin
    if Assigned(TGridWraper(sgItems).InplaceEditor) then
    begin
      if {$IFDEF FPC}TEdit({$ENDIF}TGridWraper(sgItems).InplaceEditor{$IFDEF FPC}){$ENDIF}.SelLength=Length(sgItems.Cells[sgItems.Col,sgItems.Row]) then
        sbCancel.Click
      else
        {$IFDEF FPC}TEdit({$ENDIF}TGridWraper(sgItems).InplaceEditor{$IFDEF FPC}){$ENDIF}.SelectAll
    end
    else
      sbCancel.Click;
  end
  else
  if Key=VK_RETURN then
  begin
    if ssShift in Shift then
      sgItemsDblClick(sgItems)
    else
    if Assigned(TGridWraper(sgItems).InplaceEditor) and
      ({$IFDEF FPC}TEdit({$ENDIF}TGridWraper(sgItems).InplaceEditor{$IFDEF FPC}){$ENDIF}.SelLength=Length(sgItems.Cells[sgItems.Col,sgItems.Row])) then
      sbOK.Click
  end;
end;

procedure TfrmEmbeddedEditor.sgItemsDblClick(Sender: TObject);
var
  tmp,tit:string;
begin
  if Length(sgItems.Cells[1,sgItems.Row])>0 then
  begin
    tmp:=sgItems.Cells[2,sgItems.Row];
    tit:=sgItems.Cells[1,sgItems.Row];
    if (sgItems.Objects[2,sgItems.Row]<>nil) and (TComponent(sgItems.Objects[2,sgItems.Row]).Owner is TFrame) then
      tit:=TComponent(sgItems.Objects[2,sgItems.Row]).Owner.Name+'.'+tit;
    if InputQueryMemoLM(Caption,tit,tmp,320,250) then
    begin
      if (Length(tmp)>1) and (Copy(tmp,Length(tmp)-1,2)=#13#10) then
        Delete(tmp,Length(tmp)-1,2);
      sgItemsSetEditText(sgItems, 2, sgItems.Row, tmp);
      sgItems.Cells[2,sgItems.Row]:=tmp;
    end;
  end;
end;

{$IFDEF FPC}
{$HINTS OFF}
{$ENDIF}

procedure TfrmEmbeddedEditor.sgItemsGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
  FOldValue:=Value;
end;

procedure TfrmEmbeddedEditor.sgItemsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  p:Integer;
begin
  if FOldValue<>Value then
  begin
    FOldValue:=Value;
    case Cardinal(sgItems.Objects[0,ARow]) of
      0:sgItems.Objects[0,ARow]:={$IFDEF FPC} TObject(Pointer(2)) {$ELSE} Pointer(2) {$ENDIF};
      1:sgItems.Objects[0,ARow]:={$IFDEF FPC} TObject(Pointer(3)) {$ELSE} Pointer(3) {$ENDIF};
    end;
    if Cardinal(sgItems.Objects[1,ARow])=0 then  // item kind (0-property, 1-constant)
    begin
      if (sgItems.Objects[2,ARow]=nil) or (not (TComponent(sgItems.Objects[2,ARow]).Owner is TFrame)) then
        _SetPropertyByPath(FRoot,sgItems.Cells[1,ARow],Value,FLangMan.AllowSpecialProps)
      else
      begin
        p:=Pos('.',sgItems.Cells[1,ARow]);
        if p=0 then
          _SetPropertyByPath(TComponent(sgItems.Objects[2,ARow]),sgItems.Cells[1,ARow],Value,FLangMan.AllowSpecialProps)
        else
          _SetPropertyByPath(TComponent(sgItems.Objects[2,ARow]),Copy(sgItems.Cells[1,ARow],p+1,maxint),Value,FLangMan.AllowSpecialProps)
      end;
    end;
    if not FModified then
      FModified:=True;
    {$IFDEF FPC}
    sgItems.Repaint;
    {$ENDIF}
  end;
end;

procedure TfrmEmbeddedEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult=mrOK then
    SaveItems;
end;

{$IFDEF FPC}
{$HINTS ON}
{$WARNINGS ON}
{$ENDIF}

initialization
  {$IFDEF FPC}
  {$I plsEmbedded.lrs}
  {$ENDIF}

  FShortcutRef:=nil;
  FCaptureRef:=nil;
  FCaptureHook:=0;
  FCaptureHint:=nil;
  pls_Editor_Title:='';
  pls_Editor_Width:=400;
  pls_Editor_Height:=250;
  pls_Editor_PropFilter:='Caption,Hint,Text';
  pls_Editor_OnlyPredefinedProps:=False;
  pls_Editor_IncludeConstants:=False;
  pls_Editor_SaveInUserMode:=False;
  pls_Editor_DefaultLangCode:='en';
  pls_Editor_DefaultLangName:='English';

finalization

  LE_RemoveCaptureControlShortcut;
  LE_EndCaptureControl;

end.
