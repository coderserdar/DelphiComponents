unit TB2KAddOnAutoState;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TB2Dock, TB2Toolbar, TB2ToolWindow, TB2Item, TB2MRU, TB2KAddOnCustomizeForm,
  IniFiles, Registry;

const
  // Custom message used internally
  TB2KASS_SETWDWSTATE = WM_USER;

type
  // Forward class declaration
  TTB2KAutoStateSave = class;

  TTB2KASSHook = class(TWinControl)
  private
    FTB2KAutoStateSave: TTB2KAutoStateSave;
    procedure SendMsgToOwner(var Msg: TMessage);
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
    procedure TB2KASSSetToolbarState(var Msg: TMessage); message
      TB2KASS_SETWDWSTATE;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTB2KASSStoreMethod = (smRegistry, smIniFile);
  TTB2KASSRootkey = (CURRENT_USER, LOCAL_MACHINE);

  TTB2KAutoStateSave = class(TComponent)
  private
    fHook: TTB2KASSHook;
    fFormShown: Boolean;
    fRestorePending: Boolean;
    FAutoSaveRestore: Boolean;
    FIniFileName: string;
    FSection: string;
    FRootkey: TTB2KASSRootkey;
    FSubkey: string;
    FStorage: TTB2KASSStoreMethod;
    slList: TStringList;
    bReset: Boolean;
    procedure setSection(aValue: string);
    procedure setSubkey(aValue: string);
    function getFullNamePath(AComponent: TComponent): string;
    procedure AddValue(AKey: string; AValue: string);
    function GetValue(AKey: string): string;
    procedure AddTBItems(AItem: TTBCustomItem; AKeyname: string);
    procedure GetTBItems(AItem: TTBCustomItem; AKeyname: string);
    procedure GetTB2KState;
    procedure SetTB2KState;
    procedure RestoreState;
    function parseString(AString: string): string;

  protected
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
    procedure SetParentComponent(Value: TComponent); override;
    procedure TB2KASSSetToolbarState(var Msg: TMessage); message
      TB2KASS_SETWDWSTATE;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveState;
    procedure LoadState;
    procedure ResetState;

  published
    property AutoSaveRestore: Boolean read FAutoSaveRestore write FAutoSaveRestore;
    property IniFileName: string read FIniFileName write FIniFileName;
    property Section: string read FSection write SetSection;
    property Rootkey: TTB2KASSRootkey read FRootkey write FRootkey;
    property Subkey: string read FSubkey write SetSubkey;
    property Storage: TTB2KASSStoreMethod read FStorage write FStorage;
  end;

  ETB2KAutoStateSave = class(Exception);

implementation

uses StrUtils;

{ TTB2KASSHook }

procedure TTB2KASSHook.CMShowingChanged(var Msg: TMessage);
begin
  inherited;
  SendMsgToOwner(Msg);
end;

constructor TTB2KASSHook.Create(AOwner: TComponent);
begin
  Assert(Assigned(AOwner));
  Assert(AOwner is TTB2KAutoStateSave);
  inherited;
  FTB2KAutoStateSave := AOwner as TTB2KAutoStateSave;
end;

procedure TTB2KASSHook.SendMsgToOwner(var Msg: TMessage);
begin
  FTB2KAutoStateSave.Dispatch(Msg);
end;

procedure TTB2KASSHook.WMDestroy(var Msg: TMessage);
begin
  SendMsgToOwner(Msg);
  inherited;
end;

procedure TTB2KASSHook.TB2KASSSetToolbarState(var Msg: TMessage);
begin
  inherited;
  SendMsgToOwner(Msg);
end;

{ TTB2KAutoStateSave }

resourcestring
  // Error messages
  sErrFormRequired = 'TB2KAutoStateSave components must be placed on a form';
  sErrSingleInstance = 'Only one TB2KAutoStateSave component is permitted on a '
    + 'form: %s is already present on %s';

constructor TTB2KAutoStateSave.Create(AOwner: TComponent);
var
  i: Integer;
begin
  if not Assigned(AOwner) or not (AOwner is TForm) then
    raise ETB2KAutoStateSave.Create(sErrFormRequired);
  // Ensure there is only one TTB2KAutoStateSave component on a form
  for i := 0 to Pred(AOwner.ComponentCount) do
    if AOwner.Components[i] is TTB2KAutoStateSave then
      raise ETB2KAutoStateSave.CreateFmt(sErrSingleInstance,
        [AOwner.Components[i].Name, AOwner.Name]);
  inherited Create(AOwner);
  FIniFileName := '<APP_PATH><APP_NAME>.ini';
  FSection := '<FORM_NAME>_ToolbarSettings';
  FSubkey := 'Software\<APP_NAME>\<FORM_NAME>\ToolbarSettings';
  bReset := False;
  if not (csDesigning in ComponentState) then
    fHook := TTB2KASSHook.Create(Self);

end;

destructor TTB2KAutoStateSave.Destroy;
begin
  inherited;
end;

procedure TTB2KAutoStateSave.setSection(aValue: string);
begin
  if FSection <> aValue then
  begin
    FSection := aValue;
    if Pos('<FORM_NAME>', UpperCase(FSection)) = 0 then
    begin
      FSection := '<FORM_NAME>_' + aValue;
    end;
  end;
end;

procedure TTB2KAutoStateSave.setSubkey(aValue: string);
begin
  if FSubKey <> aValue then
  begin
    FSubKey := aValue;
    if Pos('<FORM_NAME>', UpperCase(FSubKey)) = 0 then
    begin
      FSubKey := aValue + '\<FORM_NAME>';
    end;
  end;
end;

procedure TTB2KAutoStateSave.SetParentComponent(Value: TComponent);
begin
  inherited;
  if not (csDesigning in ComponentState) and (Value is TWinControl) then
    fHook.Parent := Value as TWinControl;
end;

procedure TTB2KAutoStateSave.WMDestroy(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) and AutoSaveRestore then
    SaveState;
  inherited;
end;

procedure TTB2KAutoStateSave.CMShowingChanged(var Msg: TMessage);
begin
  inherited;
  // We only act on this method the first time it's called:
  // fFormShown indicates if we've been here before
  if not fFormShown then
  begin
    // This code executed first time called only
    fFormShown := True;
    if not (csDesigning in ComponentState) and (AutoSaveRestore or
      fRestorePending) then
      PostMessage(fHook.Handle, TB2KASS_SETWDWSTATE, 0, 0);
  end;
end;

procedure TTB2KAutoStateSave.TB2KASSSetToolbarState(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) and (AutoSaveRestore) then
    RestoreState;
end;

function TTB2KAutoStateSave.getFullNamePath(AComponent: TComponent): string;
var
  comp: TComponent;
  Name: string;
begin
  Name := AComponent.Name;
  comp := AComponent;
  repeat
    comp := comp.Owner;
    Name := comp.Name + '.' + Name;
  until comp.InheritsFrom(TForm) or (comp.Owner = nil);
  Result := Name;
end;

procedure TTB2KAutoStateSave.AddValue(AKey: string; AValue: string);
begin
  slList.Add(AKey + '=' + AValue);
end;

function TTB2KAutoStateSave.GetValue(AKey: string): string;
begin
  Result := slList.Values[AKey];
end;

procedure TTB2KAutoStateSave.AddTBItems(AItem: TTBCustomItem; AKeyname: string);
var
  i: Integer;
begin
  for i := 0 to Pred(AItem.Count) do
  begin
    if AItem.Items[i].ClassNameIs('TTBControlItem') then
    begin
      AddValue(AKeyname + '.Item[' + IntToStr(i) + '].Visible',
        BoolToStr(TTBControlItem(AItem.Items[i]).Control.Visible, True));
    end
    else
    begin
      AddValue(AKeyname + '.Item[' + IntToStr(i) + '].Visible',
        BoolToStr(TTBCustomItem(AItem.Items[i]).Visible, True));
      if (AItem.Items[i].Count > 0) or (AItem.Items[i].LinkSubitems <> nil) then
      begin
        if AItem.Items[i].LinkSubitems = nil then
        begin
          AddTBItems(AItem.Items[i], AKeyname + '.Item[' + IntToStr(i) + ']');
        end
        else
        begin
          AddTBItems(AItem.Items[i].LinkSubitems, AKeyname + '.Item[' +
            IntToStr(i) + ']');
        end;
      end;
    end;
  end;
end;

procedure TTB2KAutoStateSave.GetTBItems(AItem: TTBCustomItem; AKeyname: string);
var
  i: Integer;
  sVal: string;
begin
  for i := 0 to Pred(AItem.Count) do
  begin
    sVal := getValue(AKeyname + '.Item[' + IntToStr(i) + '].Visible');
    if (sVal <> '') then
    begin
      if AItem.Items[i].ClassNameIs('TTBControlItem') then
      begin
        TTBControlItem(AItem.Items[i]).Control.Visible := StrToBool(sVal);
      end
      else
      begin
        TTBCustomItem(AItem.Items[i]).Visible := StrToBool(sVal);
      end;
    end;

    if not AItem.Items[i].ClassNameIs('TTBControlItem') then
    begin
      if (AItem.Items[i].Count > 0) or (AItem.Items[i].LinkSubitems <> nil) then
      begin
        if AItem.Items[i].LinkSubitems = nil then
        begin
          GetTBItems(AItem.Items[i], AKeyname + '.Item[' + IntToStr(i) + ']');
        end
        else
        begin
          GetTBItems(AItem.Items[i].LinkSubitems, AKeyname + '.Item[' +
            IntToStr(i) + ']');
        end;
      end;
    end;
  end;
end;

procedure TTB2KAutoStateSave.GetTB2KState;
var
  i, j: Integer;
  sName: string;
begin
  slList.Clear;
  for i := 0 to Pred(Owner.ComponentCount) do
  begin
    // TB2KCustomizeDialog
    if Owner.Components[i].ClassNameIs('TTB2KCustomizeDialog') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      AddValue(sName + '.Bitmapcount',
        IntToStr(TTB2KCustomizeDialog(Owner.Components[i]).BitmapFileNames.Count));
      for j := 0 to
        Pred(TTB2KCustomizeDialog(Owner.Components[i]).BitmapFileNames.Count) do
      begin
        AddValue(sName + '.Control[' + IntToStr(j) + ']',
          TTB2KCustomizeDialog(Owner.Components[i]).BitmapFileNames.Names[j]);
        AddValue(sName + '.Bitmap[' + IntToStr(j) + ']',
          TTB2KCustomizeDialog(Owner.Components[i]).BitmapFileNames.ValueFromIndex[j]);
      end;
    end;

    // TBDock
    if Owner.Components[i].ClassNameIs('TTBDock') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      AddValue(sName + '.LimitToOneRow',
        BoolToStr(TTBDock(Owner.Components[i]).LimitToOneRow, True));
      AddValue(sName + '.AllowDrag',
        BoolToStr(TTBDock(Owner.Components[i]).AllowDrag, True));
      AddValue(sName + '.BackgroundOnToolbars',
        BoolToStr(TTBDock(Owner.Components[i]).BackgroundOnToolbars, True));
      if TTBDock(Owner.Components[i]).Background = nil then
      begin
        AddValue(sName + '.Background', 'nil');
      end
      else
      begin
        AddValue(sName + '.Background',
          TTBDock(Owner.Components[i]).Background.Name);
      end;
    end;

    // TBToolbar
    if Owner.Components[i].ClassNameIs('TTBToolbar') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      AddValue(sName + '.Visible',
        BoolToStr(TTBToolbar(Owner.Components[i]).Visible, True));
      AddValue(sName + '.Floating',
        BoolToStr(TTBToolbar(Owner.Components[i]).Floating, True));
      AddValue(sName + '.FloatingPosition.X',
        IntToStr(TTBToolbar(Owner.Components[i]).FloatingPosition.X));
      AddValue(sName + '.FloatingPosition.Y',
        IntToStr(TTBToolbar(Owner.Components[i]).FloatingPosition.Y));
      AddValue(sName + '.FloatingWidth',
        IntToStr(TTBToolbar(Owner.Components[i]).FloatingWidth));
      if TTBToolbar(Owner.Components[i]).CurrentDock <> nil then
      begin
        AddValue(sName + '.CurrentDock',
          TTBToolbar(Owner.Components[i]).CurrentDock.Name);
      end
      else
      begin
        AddValue(sName + '.CurrentDock', '');
      end;
      AddValue(sName + '.DockPos',
        IntToStr(TTBToolbar(Owner.Components[i]).DockPos));
      AddValue(sName + '.DockRow',
        IntToStr(TTBToolbar(Owner.Components[i]).DockRow));
      if TTBToolbar(Owner.Components[i]).LastDock <> nil then
      begin
        AddValue(sName + '.LastDock',
          TTBToolbar(Owner.Components[i]).LastDock.Name);
      end
      else
      begin
        AddValue(sName + '.LastDock', '');
      end;
      AddValue(sName + '.Left', IntToStr(TTBToolbar(Owner.Components[i]).Left));
      // TBToolbar-Icons
      if TTBToolbar(Owner.Components[i]).LinkSubitems = nil then
      begin
        AddValue(sName + '.ImageAboveCaption', BoolToStr(tboImageAboveCaption in
          TTBToolbar(Owner.Components[i]).Options, True));
        AddTBItems(TTBCustomItem(TTBToolbar(Owner.Components[i]).Items), sName);
      end
      else
      begin
        AddValue(sName + '.ImageAboveCaption', BoolToStr(tboImageAboveCaption in
          TTBToolbar(Owner.Components[i]).LinkSubitems.Options, True));
        AddTBItems(TTBCustomItem(TTBToolbar(Owner.Components[i]).LinkSubitems),
          sName);
      end;
    end;

    // TBToolWindow
    if Owner.Components[i].ClassNameIs('TTBToolWindow') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      AddValue(sName + '.Visible',
        BoolToStr(TTBToolWindow(Owner.Components[i]).Visible, True));
      AddValue(sName + '.Floating',
        BoolToStr(TTBToolWindow(Owner.Components[i]).Floating, True));
      AddValue(sName + '.FloatingPosition.X',
        IntToStr(TTBToolWindow(Owner.Components[i]).FloatingPosition.X));
      AddValue(sName + '.FloatingPosition.Y',
        IntToStr(TTBToolWindow(Owner.Components[i]).FloatingPosition.Y));
      AddValue(sName + '.Width',
        IntToStr(TTBToolWindow(Owner.Components[i]).Width));
      AddValue(sName + '.Height',
        IntToStr(TTBToolWindow(Owner.Components[i]).Height));
      if TTBToolWindow(Owner.Components[i]).CurrentDock <> nil then
      begin
        AddValue(sName + '.CurrentDock',
          TTBToolWindow(Owner.Components[i]).CurrentDock.Name);
      end
      else
      begin
        AddValue(sName + '.CurrentDock', '');
      end;
      AddValue(sName + '.DockPos',
        IntToStr(TTBToolWindow(Owner.Components[i]).DockPos));
      AddValue(sName + '.DockRow',
        IntToStr(TTBToolWindow(Owner.Components[i]).DockRow));
      if TTBToolWindow(Owner.Components[i]).LastDock <> nil then
      begin
        AddValue(sName + '.LastDock',
          TTBToolWindow(Owner.Components[i]).LastDock.Name);
      end
      else
      begin
        AddValue(sName + '.LastDock', '');
      end;
      AddValue(sName + '.Left',
        IntToStr(TTBToolWindow(Owner.Components[i]).Left));
    end;

    // TBMRUList
    if Owner.Components[i].ClassNameIs('TTBMRUList') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      AddValue(sName + '.Itemcount',
        IntToStr(TTBMRUList(Owner.Components[i]).items.Count));
      for j := 0 to Pred(TTBMRUList(Owner.Components[i]).items.Count) do
      begin
        AddValue(sName + '.Item[' + IntToStr(j) + ']',
          TTBMRUList(Owner.Components[i]).items.Strings[j]);
      end;
    end;
  end;
end;

procedure TTB2KAutoStateSave.SetTB2KState;
var
  i, j, k: Integer;
  sName, sVal, sHelp: string;
  pFloat: TPoint;
  tbDock: TTBDock;
  itOptions: TTBItemOptions;
  background: TTBBackground;
  bitmapFileName: TStringList;
begin
  for i := 0 to Pred(Owner.ComponentCount) do
  begin
    // TB2KCustomizeDialog
    if Owner.Components[i].ClassNameIs('TTB2KCustomizeDialog') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      sVal := getValue(sName + '.Bitmapcount');
      if (sVal <> '') then
      begin
        k := StrToIntDef(sVal, 0);
        if k > 0 then
        begin
          bitmapFileName := TStringList.Create;
          for j := 0 to k do
          begin
            sHelp := getValue(sName + '.Control[' + IntToStr(j) + ']');
            background := TTBBackground(Owner.FindComponent(sHelp));
            if background <> nil then
            begin
              sVal := getValue(sName + '.Bitmap[' + IntToStr(j) + ']');
              if sVal <> '' then
              begin
                background.Bitmap.LoadFromFile(sVal);
                bitmapFileName.Append(sHelp + '=' + sVal);
              end;
            end;
          end;
          TTB2KCustomizeDialog(Owner.Components[i]).BitmapFileNames :=
            bitmapFileName;
        end;
      end;
    end;

    // TBDock
    if Owner.Components[i].ClassNameIs('TTBDock') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      sVal := getValue(sName + '.LimitToOneRow');
      if (sVal <> '') then
        TTBDock(Owner.Components[i]).LimitToOneRow := StrToBool(sVal);
      sVal := getValue(sName + '.AllowDrag');
      if (sVal <> '') then
        TTBDock(Owner.Components[i]).AllowDrag := StrToBool(sVal);
      sVal := getValue(sName + '.BackgroundOnToolbars');
      if (sVal <> '') then
        TTBDock(Owner.Components[i]).BackgroundOnToolbars := StrToBool(sVal);
      sVal := getValue(sName + '.Background');
      if (sVal <> '') then
        TTBDock(Owner.Components[i]).Background :=
          TTBBasicBackground(Owner.FindComponent(sVal));
    end;

    // TBToolbar
    if Owner.Components[i].ClassNameIs('TTBToolbar') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      if (TTBToolbar(Owner.Components[i]).CloseButton)
        or (TTBToolbar(Owner.Components[i]).CloseButtonWhenDocked) then
      begin
        sVal := getValue(sName + '.Visible');
        if (sVal <> '') then
          TTBToolbar(Owner.Components[i]).Visible := StrToBool(sVal);
      end;
      if (TTBToolbar(Owner.Components[i]).DockMode = dmCanFloat) then
      begin
        sVal := getValue(sName + '.FloatingPosition.X');
        if (sVal <> '') then
          pFloat.X := StrToInt(sVal);
        sVal := getValue(sName + '.FloatingPosition.Y');
        if (sVal <> '') then
          pFloat.Y := StrToInt(sVal);
        TTBToolbar(Owner.Components[i]).FloatingPosition := pFloat;
        sVal := getValue(sName + '.FloatingWidth');
        if (sVal <> '') then
          TTBToolbar(Owner.Components[i]).FloatingWidth := StrToInt(sVal);
        sVal := getValue(sName + '.Floating');
        if (sVal <> '') then
          TTBToolbar(Owner.Components[i]).Floating := StrToBool(sVal);
      end;
      if (TTBToolbar(Owner.Components[i]).DockMode <> dmCannotFloatOrChangeDocks)
      then
      begin
        if not TTBToolbar(Owner.Components[i]).Floating then
        begin
          sVal := getValue(sName + '.CurrentDock');
          if (sVal <> '') then
          begin
            tbDock := TTBDock(Owner.FindComponent(sVal));
            if tbDock <> nil then
              TTBToolbar(Owner.Components[i]).CurrentDock := tbDock;
          end;
        end;
        sVal := getValue(sName + '.LastDock');
        if (sVal <> '') then
        begin
          tbDock := TTBDock(Owner.FindComponent(sVal));
          if tbDock <> nil then
            TTBToolbar(Owner.Components[i]).LastDock := tbDock;
        end;
      end;
      sVal := getValue(sName + '.DockPos');
      if (sVal <> '') then
        TTBToolbar(Owner.Components[i]).DockPos := StrToInt(sVal);
      sVal := getValue(sName + '.DockRow');
      if (sVal <> '') then
        TTBToolbar(Owner.Components[i]).DockRow := StrToInt(sVal);
      sVal := getValue(sName + '.Left');
      if (sVal <> '') then
        TTBToolbar(Owner.Components[i]).Left := StrToInt(sVal);

      // TBToolbar-Icons
      if TTBToolbar(Owner.Components[i]).LinkSubitems = nil then
      begin
        sVal := getValue(sName + '.ImageAboveCaption');
        if (sVal = 'True') then
        begin
          itOptions := TTBToolbar(Owner.Components[i]).Options;
          Include(itOptions, tboImageAboveCaption);
          TTBToolbar(Owner.Components[i]).Options := itOptions;
        end
        else
        begin
          itOptions := TTBToolbar(Owner.Components[i]).Options;
          Exclude(itOptions, tboImageAboveCaption);
          TTBToolbar(Owner.Components[i]).Options := itOptions;
        end;
        GetTBItems(TTBCustomItem(TTBToolbar(Owner.Components[i]).Items), sName);
      end
      else
      begin
        sVal := getValue(sName + '.ImageAboveCaption');
        if (sVal = 'True') then
        begin
          itOptions := TTBToolbar(Owner.Components[i]).LinkSubitems.Options;
          Include(itOptions, tboImageAboveCaption);
          TTBToolbar(Owner.Components[i]).LinkSubitems.Options := itOptions;
        end
        else
        begin
          itOptions := TTBToolbar(Owner.Components[i]).LinkSubitems.Options;
          Exclude(itOptions, tboImageAboveCaption);
          TTBToolbar(Owner.Components[i]).LinkSubitems.Options := itOptions;
        end;
        GetTBItems(TTBCustomItem(TTBToolbar(Owner.Components[i]).LinkSubitems),
          sName);
      end;
    end;

    // TBToolWindow
    if Owner.Components[i].ClassNameIs('TTBToolWindow') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      if (TTBToolWindow(Owner.Components[i]).CloseButton)
        or (TTBToolWindow(Owner.Components[i]).CloseButtonWhenDocked) then
      begin
        sVal := getValue(sName + '.Visible');
        if (sVal <> '') then
          TTBToolWindow(Owner.Components[i]).Visible := StrToBool(sVal);
      end;
      if (TTBToolWindow(Owner.Components[i]).DockMode = dmCanFloat) then
      begin
        sVal := getValue(sName + '.FloatingPosition.X');
        if (sVal <> '') then
          pFloat.X := StrToInt(sVal);
        sVal := getValue(sName + '.FloatingPosition.Y');
        if (sVal <> '') then
          pFloat.Y := StrToInt(sVal);
        TTBToolWindow(Owner.Components[i]).FloatingPosition := pFloat;
        sVal := getValue(sName + '.Floating');
        if (sVal <> '') then
          TTBToolWindow(Owner.Components[i]).Floating := StrToBool(sVal);
      end;
      if (TTBToolWindow(Owner.Components[i]).DockMode <>
        dmCannotFloatOrChangeDocks) then
      begin
        if not TTBToolWindow(Owner.Components[i]).Floating then
        begin
          sVal := getValue(sName + '.CurrentDock');
          if (sVal <> '') then
          begin
            tbDock := TTBDock(Owner.FindComponent(sVal));
            if tbDock <> nil then
              TTBToolWindow(Owner.Components[i]).CurrentDock := tbDock;
          end;
        end;
        sVal := getValue(sName + '.LastDock');
        if (sVal <> '') then
        begin
          tbDock := TTBDock(Owner.FindComponent(sVal));
          if tbDock <> nil then
            TTBToolWindow(Owner.Components[i]).LastDock := tbDock;
        end;
      end;
      sVal := getValue(sName + '.Width');
      if (sVal <> '') then
        TTBToolWindow(Owner.Components[i]).Width := StrToInt(sVal);
      sVal := getValue(sName + '.Height');
      if (sVal <> '') then
        TTBToolWindow(Owner.Components[i]).Height := StrToInt(sVal);
      sVal := getValue(sName + '.DockPos');
      if (sVal <> '') then
        TTBToolWindow(Owner.Components[i]).DockPos := StrToInt(sVal);
      sVal := getValue(sName + '.DockRow');
      if (sVal <> '') then
        TTBToolWindow(Owner.Components[i]).DockRow := StrToInt(sVal);
      sVal := getValue(sName + '.Left');
      if (sVal <> '') then
        TTBToolWindow(Owner.Components[i]).Left := StrToInt(sVal);
    end;

    // TBMRUList
    if Owner.Components[i].ClassNameIs('TTBMRUList') then
    begin
      sName := getFullNamePath(Owner.Components[i]);
      sVal := getValue(sName + '.Itemcount');
      if (sVal <> '') then
      begin
        k := StrToIntDef(sVal, 0);
        if k > 0 then
        begin
          TTBMRUList(Owner.Components[i]).Items.Clear;
          for j := 0 to Pred(k) do
          begin
            sVal := getValue(sName + '.Item[' + IntToStr(j) + ']');
            TTBMRUList(Owner.Components[i]).Items.Add(sVal);
          end;
        end;
      end;
    end;
  end;
end;

function TTB2KAutoStateSave.parseString(AString: string): string;
var
  s, e, l: Integer;
  sHelp, sToken: string;
begin
  sHelp := AString;
  repeat
    s := Pos('<', sHelp);
    if s > 0 then
    begin
      e := PosEx('>', sHelp, s);
      if e > 0 then
      begin
        l := e - s + 1;
        sToken := UpperCase(Copy(sHelp, s, l));
        if sToken = '<FORM_NAME>' then
        begin
          Delete(sHelp, s, l);
          Insert(Owner.Name, sHelp, s);
        end;
        if sToken = '<APP_NAME>' then
        begin
          Delete(sHelp, s, l);
          Insert(Copy(ExtractFileName(Application.ExeName), 1,
            Length(ExtractFileName(Application.ExeName)) - 4), sHelp, s);
        end;
        if sToken = '<APP_PATH>' then
        begin
          Delete(sHelp, s, l);
          Insert(ExtractFilePath(Application.ExeName), sHelp, s);
        end;
      end;
    end;
  until s <= 0;

  Result := sHelp;
end;

procedure TTB2KAutoStateSave.SaveState;
var
  Reg: TRegistry;
  Ini: TIniFile;
  i: Integer;
begin
  Ini := nil;
  slList := TStringList.Create;
  GetTB2KState;

  if FStorage = smRegistry then
  begin
    Reg := TRegistry.Create;
    try
      if FRootkey = CURRENT_USER then
      begin
        Reg.RootKey := HKEY_CURRENT_USER;
      end
      else
      begin
        Reg.RootKey := HKEY_LOCAL_MACHINE;
      end;
      Reg.DeleteKey(parseString(FSubkey));
      if (slList.Count > 0) and not (bReset) then
      begin
        if Reg.OpenKey(parseString(FSubkey), True) then
        begin
          for i := 0 to Pred(slList.Count) do
          begin
            Reg.WriteString(slList.Names[i], slList.ValueFromIndex[i]);
          end;
          Reg.CloseKey;
        end;
      end;
    finally
      Reg.Free;
    end;
  end
  else
  begin
    try
      Ini := TIniFile.Create(parseString(FIniFileName));
      Ini.EraseSection(parseString(FSection));
      if (slList.Count > 0) and not (bReset) then
      begin
        for i := 0 to Pred(slList.Count) do
        begin
          Ini.WriteString(parseString(FSection), slList.Names[i],
            slList.ValueFromIndex[i]);
        end;
      end;
    finally
      Ini.Free;
    end;
  end;
  slList.Free;
end;

procedure TTB2KAutoStateSave.RestoreState;
begin
  Assert(not (csDesigning in ComponentState));

  // If not ready to act on restore command set pending flag for later handling
  if not fFormShown then
  begin
    // note that a restore is pending
    fRestorePending := True;
    Exit;
  end;

  // We are actually doing the restoration: note that not pending any more
  fRestorePending := False;
  LoadState;
end;

procedure TTB2KAutoStateSave.LoadState;
var
  Reg: TRegistry;
  Ini: TIniFile;
  i: Integer;
begin
  Ini := nil;
  slList := TStringList.Create;
  if FStorage = smRegistry then
  begin
    Reg := TRegistry.Create;
    try
      if FRootkey = CURRENT_USER then
      begin
        Reg.RootKey := HKEY_CURRENT_USER;
      end
      else
      begin
        Reg.RootKey := HKEY_LOCAL_MACHINE;
      end;

      if Reg.OpenKey(parseString(FSubkey), False) then
      begin
        Reg.GetValueNames(slList);
        for i := 0 to Pred(slList.Count) do
        begin
          slList.Strings[i] := slList.Strings[i] + '=' +
            Reg.ReadString(slList.Strings[i]);
        end;
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  end
  else
  begin
    try
      begin
        Ini := TIniFile.Create(parseString(FIniFileName));
        Ini.ReadSectionValues(parseString(FSection), slList);
      end;
    finally
      Ini.Free;
    end;
  end;

  if slList.Count > 0 then
  begin
    SetTB2KState;
  end;
  slList.Free;
end;

procedure TTB2KAutoStateSave.ResetState;
begin
  bReset := True;
end;

end.

