unit FxButton;

interface

uses
  Windows, Variants, Messages, SysUtils, Classes, Graphics, Grids, Buttons, Controls,
  StdCtrls, Forms, Dialogs, DB, Menus, ExtCtrls,
  FxConsts, FxDB, FxStore, FxCommon;

type
  TMenuProp = (tmChecked, tmRadio, tmNone);

  TQuickMenuItem = class(TMenuItem)
  private
    Action: Integer;
  end;

  TQuickMenu = class(TPopUpMenu)
  private
    FOnSelected: TNotifyEvent;
    procedure EHOnItemClick(Sender: TObject);
  public
    FAction: Integer;
    iDim: Integer;
    dimGroup: TDimGroup;
    isGroupStart: Boolean;
    Index: Integer;
    Cell: Integer;
    ValueIndex: Integer;
    procedure Clear;
    procedure SetTitle(value: String);
    procedure AddLine(const value: String; Prop: TMenuProp; Action: Integer);
    procedure PopUpAtMe(aControl: TWinControl; x,y: Integer);
    property OnSelected: TNotifyEvent read FOnSelected write FonSelected;
  end;

  TPivotButtonMouseState = (xmNone, xmPushed, xmDragging);
  TPivotButtonType = (pbDimension, pbTarget, pbSummary, pbInactive);

  TPivotButton = class(TSpeedButton)
  private
    FType: TPivotButtonType;
    FSource: TFxSource;
    FMenu: TQuickMenu;
    FMouseState: TPivotButtonMouseState;
    SaveX: Integer;
    SaveY: Integer;
    myDim: Integer;
    myDimInfo: TDimInfo;
    procedure SetState(Value: TPivotButtonMouseState);
    procedure SetMyDim(iDim: Integer);
    procedure SetDecisionSource(Value: TFxSource);
    procedure SelectButtonValue;
    procedure SelectButtonProperties;
    procedure EHOnValue(Sender: TObject);
    procedure EHOnProperty(Sender: TObject);
  protected
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DragCanceled; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property DimInfo:TDimInfo read myDimInfo;
    procedure NewState;
    procedure SetType(Value: TPivotButtonType);
  published
    property Parent;
    property DecisionSource: TFxSource read FSource write SetDecisionSource;
    property iDim: Integer read myDim write SetMyDim;
  end;

  TDecisionButtonPosition = (xtHorizontal, xtVertical, xtLeftTop);
  TDecisionButtonGrouping = (xtCheck, xtRadio, xtSequential);

  TDecisionPivotOption = (xtRows, xtColumns, xtSummaries);
  TDecisionPivotOptions = set of TDecisionPivotOption;

implementation

uses FxPivSrc, FxCache, FxMap;

const
  crDimMove = 100;
  crDimIns = 101;
  bmpRows = 102;
  bmpCols = 103;

  { TPivotButton methods }

constructor TPivotButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetState(xmNone);
  AllowAllUp := True;
  Enabled := True;
  Caption := '';
  GroupIndex := 0;
  Screen.Cursors[crDimMove] := LoadCursor(HInstance, 'DIMMOVE');
  Screen.Cursors[crDimIns] := LoadCursor(HInstance, 'DIMINS');
end;

destructor TPivotButton.Destroy;
begin
  FMenu.free;
  FMenu := nil;
  inherited Destroy;
end;

procedure TPivotButton.SetDecisionSource(Value: TFxSource);
begin
  if (Value <> FSource) then FSource := Value;
end;

procedure TPivotButton.SetMyDim(iDim: Integer);
begin
  myDim := iDim;
  NewState;
end;

procedure TPivotButton.SetType(Value: TPivotButtonType);
begin
  FType := Value;
end;

procedure TPivotButton.NewState;
begin
  if (myDim >= 0) and assigned(FSource) and (myDim <= FSource.nDims) then
  begin
    myDimInfo.iGroup := FSource.Pivot[myDim].iGroup;
    myDimInfo.iValue := FSource.Pivot[myDim].iValue;
    myDimInfo.iState := FSource.Pivot[myDim].iState;
    myDimInfo.iIndex := FSource.Pivot[myDim].iIndex;
    myDimInfo.iActiveIndex := FSource.Pivot[myDim].iActiveIndex;
    FType := pbDimension;
  end
  else
  begin
    Caption := '';
    myDimInfo.IIndex := -1;
    myDimInfo.IActiveIndex := -1;
  end;
  flat := (myDimInfo.IState in [dmDrilled, dmPaged]) or (FType in [pbSummary, pbTarget, pbInactive]);

  if (MyDimInfo.IState = dmOpen) then
    Down := True
  else
    Down := False;
  SetState(xmNone);
  Invalidate;
end;

procedure TPivotButton.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  if (Msg.Pos.X>Width shr 2) and (msg.Pos.x<width-width shr 2) and
  (msg.Pos.Y>Height shr 2) and (msg.Pos.Y<Height-height shr 2) then
    Msg.Result := 1
  else
    msg.Result := 0;
end;

procedure TPivotButton.Click;
begin
  case FType of
    pbSummary,
    pbInactive:    SelectButtonValue;
    pbDimension:
      if (myDimInfo.IState in [dmDrilled, dmPaged]) then
        SelectButtonValue
      else if (FMouseState <> xmDragging) then
        DecisionSource.ToggleDimIndex(myDimInfo.iGroup, myDimInfo.IIndex, False);
  end;
end;

procedure TPivotButton.Mouseup(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FType = pbDimension) and (Button = mbRight) then
  begin
    SelectButtonProperties;
  end;
  if (Ftype <> pbTarget) and (Button = mbLeft) and (FMouseState = xmPushed) then
    Click;
  SetState(xmNone);
end;

procedure TPivotButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FType = pbTarget) then Exit;
  if (Button = mbLeft) then
  begin
    SetState(xmPushed);
    SaveX := X;
    SaveY := Y;
  end;
end;

procedure TPivotButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FMouseState = xmPushed) and (FType = pbDimension) then
  begin
    if (abs(X-SaveX)>5) or (abs(SaveY-Y)>5) or (X=0) or (X=Width) or (Y=0) or (Y= Height) then
    begin
      SetState(xmDragging);
      BeginDrag(True);
    end;
  end;
end;

procedure TPivotButton.DragCanceled;
begin
  SetState(xmNone);
end;

procedure TPivotButton.SetState;
begin
  FMouseState := value;
end;

procedure TPivotButton.SelectButtonProperties;
begin
  if (FType = pbDimension) then
  begin
    if not (myDimInfo.iGroup in [dgRow, dgCol]) then
      Exit;
    if not assigned (FMenu) then
      FMenu := TQuickMenu.Create(self);
    FMenu.Clear;
    if (mydiminfo.iGroup = dgRow) then
      FMenu.AddLine(sMoveToCol, tmNone, 0)
    else if (myDimInfo.iGroup = dgCol) then
      FMenu.AddLine(sMoveToRow, tmNone, 0);
    if (myDimInfo.iState <> dmPaged) then
    begin
      if (myDimInfo.IState <> dmDrilled) then
        FMenu.AddLine(sDrilled, tmNone, 1)
      else
        FMenu.AddLine(sDrilled, tmChecked, 1);
    end;
    FMenu.OnSelected := EHOnProperty;
    FMenu.PopUpAtMe(TWinControl(self),0,Height);
  end;
end;

procedure TPivotButton.SelectButtonValue;
var
  action,i,j,limit: Integer;
  aVariant: variant;
  DM: TFxMapItem;
  S : TAbstractSummary;
begin
  if not assigned(DecisionSource) then Exit;
  if FType=pbSummary then begin
    if not assigned (FMenu) then
      FMenu := TQuickMenu.Create(self);
    FMenu.Clear;
    for i := 0 to DecisionSource.nSums-1 do begin
      S:=DecisionSource.DecisionCube.DataCache.Summaries[I];
      if S.Def.Visible then
        FMenu.AddLine(S.Name, tmNone, i);
    end;
    FMenu.OnSelected := EHOnValue;
    FMenu.PopUpAtMe(TWinControl(self),0,Height);
  end else if Ftype=pbInactive then begin
    if not assigned (FMenu) then
      FMenu := TQuickMenu.Create(self);
    FMenu.Clear;
    for i := 0 to DecisionSource.DecisionCube.DimensionMap.count-1 do
    begin
      DM := DecisionSource.DecisionCube.DimensionMap[i];
      if (not DM.Active) and (DM.ActiveFlag <> diInActive) then
      begin
        FMenu.AddLine(DM.FieldName, tmNone, i);
      end;
    end;
    FMenu.OnSelected := EHOnValue;
    FMenu.PopUpAtMe(TWinControl(self),0,Height);
  end else if FType=pbDimension then begin
    if not assigned (FMenu) then
      FMenu := TQuickMenu.Create(self);
    FMenu.Clear;
    limit:=DecisionSource.GetDimensionMemberCount(myDim);
    if myDimInfo.IState=dmPaged then begin
      action := 2;
      for i := 0 to DecisionSource.GetDimensionMemberCount(myDim)-1 do begin
        DM := DecisionSource.DecisionCube.DimensionMap[myDim];
        aVariant := DecisionSource.GetMemberAsVariant(myDim, i);
        aVariant := DM.GetBinValues(aVariant);
        if (VarType(aVariant) < varArray) then
          FMenu.AddLine(FormatVariant(aVariant, ''), tmNone, Action)
        else begin
          for j := VarArrayLowBound(aVariant,1) to VarArrayHighBound(aVariant,1) do begin
            FMenu.AddLine(FormatVariant(aVariant[j], ''), tmNone, Action);
            action := action + 1;
          end;
        end;
      end;
    end else begin
      FMenu.AddLine(sMakeDimOpen, tmNone, 0);
      FMenu.AddLine(SAllValues, tmNone, 1);
      FMenu.AddLine('-', tmNone, -1);
      for i:=0 to limit-1 do begin
        FMenu.AddLine(DecisionSource.GetMemberAsString(myDim,i), tmNone, i+2);
        //(Owner as TDecisionPivot).DrillList.AddItem(DecisionSource.GetMemberAsString(myDim,i),nil);
      end;
    end;
    FMenu.OnSelected := EHOnValue;
    FMenu.PopUpAtMe(TWinControl(self),0,Height);
  end;
end;

procedure TPivotButton.EHOnValue(Sender: TObject);
var
  DM: TFxMapItem;
  myMap: TFxMap;
  i,j: integer;
  aVariant, bVariant: variant;
  action: integer;
begin
  Action := FMenu.FAction;
  FMenu.free;
  FMenu := nil;
  if (FType = pbSummary) then
    DecisionSource.SetCurrentSummary(Action)
  else if FType = pbInactive then begin
    with DecisionSource,DecisionCube.DataCache,Pivot do begin
      DecisionCube.DimensionMap[Action].Active:=True;
      for I:=0 to Dimensions.Count-1 do
        if Items[I].iState=dmClosed then
           Dimensions[I].Def.Active:=False;
    end;
    try
      DecisionSource.DecisionCube.Refresh(nil, True);
    except
      on E: EFxMapError do
        raise Exception.create(sCouldNotOpen + E.message);
    else
      raise;
    end;
  end
  else if (FType = pbDimension) then
  begin
    if (Action = 0) then
    begin
      DecisionSource.ToggleDimIndex(myDimInfo.iGroup, myDimInfo.IIndex, False);
    end
    else if (Action = 1) then
    begin
      DecisionSource.DrillDimIndex(myDimInfo.iGroup, myDimInfo.iIndex, -1, False);
    end
    else if (myDimInfo.iState = dmPaged) then
    begin
      Action := Action - 2;
      myMap := TFxMap.Create(DecisionSource.DecisionCube, TFxMapItem);
      try
        myMap.Assign(DecisionSource.DecisionCube.DimensionMap);
        DM := myMap[myDim];
        for i := 0 to DecisionSource.GetDimensionMemberCount(myDim)-1 do
        begin
          aVariant := DecisionSource.GetMemberAsVariant(myDim, i);
          aVariant := DM.GetBinValues(aVariant);
          if (VarType(aVariant) < varArray) then
            Action := Action - 1
          else
          begin
            for j := VarArrayLowBound(aVariant,1) to VarArrayHighBound(aVariant,1) do
            begin
              action := action - 1;
              if (Action < 0) then
              begin
                bVariant := aVariant[j];
                aVariant := bVariant;
                break;
              end;
            end;
          end;
          if (Action < 0) then
            break;
        end;
        DM.StartValue := FormatVariant(aVariant,'');
        try
          DecisionSource.DecisionCube.Refresh(myMap, True );
        except
          on E: EFxMapError do
          begin
            raise exception.create(sCouldNotOpen + E.message);
          end;
        end;
      finally
        myMap.free;
      end;
      Exit;
    end
    else
      DecisionSource.DrillDimIndex(myDimInfo.iGroup, myDimInfo.iIndex, Action-2, False);
  end;
end;

procedure TPivotButton.EHOnProperty(Sender: TObject);
var
  toGroup: TDimGroup;
begin
  try
    if (FType = pbDimension) then
    begin
      if assigned(FMenu) then
      begin
        if (FMenu.FAction = 0) then
        begin
          if (myDimInfo.iGroup = dgRow) then
            toGroup := dgCol
          else
            toGroup := dgRow;
          FSource.MoveDimIndexes(myDimInfo.iGroup, toGroup, myDimInfo.IIndex, 0, False);
        end
        else if (FMenu.FAction = 1) then
        begin
          if (myDimInfo.Istate = dmDrilled) then
            DecisionSource.ToggleDimIndex(mydiminfo.iGroup, myDimInfo.IIndex, False)
          else
            DecisionSource.DrillDimIndex(mydiminfo.iGroup, myDimInfo.iIndex, -1, False);
        end;
      end;
    end;
  finally
    FMenu.Free;
    FMenu := nil;
  end;
end;

procedure TPivotButton.Paint;
var
  mid, split, x,y: Integer;
  FBmp: TBitMap;
  aRect: TRect;
  fString, string2: ShortString;
  sHeight,sMargin: Integer;
  i,ArrowX: Integer;
  ArrowString: ShortString;
  aChar: char;
  Map: TFxMap;
begin
  inherited;
  if not assigned(FSource) then Exit;
  ARect.Left := 0;
  ARect.Right := Width;
  ARect.Top := 0;
  ARect.Bottom := Height;
  if (FType = pbDimension) or (FType = pbInactive) or (FType = pbSummary) or (FType = pbTarget) then
    with Canvas do
    begin
      if (Ftype = pbTarget) then
      begin
        FBmp := TBitmap.Create;
        try
          if (mydiminfo.iGroup = dgRow) then
            FBmp.LoadFromResourceName(HInstance, 'Rows')
          else
            FBmp.LoadFromResourceName(HInstance, 'Cols');
          x := (ARect.Right-FBMP.Width) div 2;
          y := (ARect.Bottom - FBMP.Height) div 2;
          BrushCopy(Rect(x, y, x+FBmp.width, y+FBmp.height), FBMP, Rect(0,0,FBmp.Width,FBmp.Height), clMaroon);
        finally
          FBmp.Free;
        end;
        Exit;
      end;
      sHeight := TextHeight('XXX');
      if (FType = pbInactive) then
      begin
        string2 := '';
        arrowString := '6';
        fString := sActivateLabel;;
      end
      else if (Ftype = pbSummary) then
      begin
        string2 := '';
        arrowString := '6';
        i := FSource.CurrentSum;
        fString := FSource.GetSummaryName(i);
      end
      else if (FType = pbDimension) then
      begin
        if (myDimInfo.IState = dmPaged) then
        begin
          Map := FSource.DecisionCube.DimensionMap;
          String2 := FSource.GetDimensionName(myDim) + '=';
          if Assigned(Map[myDim].BinData) then
            fString := FormatVariant(Map[myDim].BinData.GetIBinValue(0,0), '')
          else
            fString := '';
          arrowString := '';
        end
        else if (myDimInfo.IState = dmDrilled) then
        begin
          String2 := FSource.GetDimensionName(myDim) + '=';
          if (myDimInfo.IValue >= 0) then
            fString := FSource.GetMemberAsString(myDim, myDimInfo.IValue)
          else
            fString := SAllValues;
          arrowString := '6';
        end
        else
        begin
          string2 := '';
          arrowString := '';
          if (myDim >= 0) then
            fString := FSource.GetDimensionName(myDim);
        end;
      end;
      if (TextWidth(FString+ArrowString) > (Width-4))
      and (string2 = '') and (Height > ((sHeight*3) div 2)) then
      begin
        mid := length(fString) div 2;
        split := 0;
        for i := length(fString) downto 2 do
        begin
          aChar := fString[i];
          if (aChar < 'A') or  ((aChar>'Z') and (aChar<'a')) or (aChar>'z') then
          begin
            if abs(mid-i) < abs(mid-split) then
              split := i;
          end;
        end;
        if (split = 0) then
          for i := length(fString) downto 2 do
          begin
            if (fString[i] <= 'Z') and (fString[i-1] > 'Z') then
            begin
              if abs(mid-i) < abs(mid-split) then
                split := i;
            end;
          end;
        if (split > 0) then
        begin
          string2 := Copy(fString,1, split-1);
          if (fString[split] = ' ') then
            split := split + 1;
          fString := Copy(fString,split, length(fString));
        end;
      end;
      while (TextWidth(fString+ArrowString) > (Width-4)) and (Length(fString) > 0) do
        Delete(fString, Length(fString), 1);
      while (TextWidth(string2) > (Width-4)) do
        Delete(String2, Length(String2), 1);
      x := ARect.Right-ARect.Left-TextWidth(FString+ArrowString);
      if (x <= 0) then
        x := ARect.Left
      else
        x := ARect.Left + (x div 2);
      ArrowX := x + TextWidth(fString);
      sMargin := (Height-2*(sHeight)) div 2;
      if (sMargin >= 0) and (string2 <> '') then
      begin
        y := ARect.Bottom - sHeight - (sMargin);
      	TextOut(x, y, fString);
        x := Width-TextWidth(String2);
        if (x <= 0) then
          x := ARect.Left
        else
          x := ARect.Left + (x div 2);
        TextOut(x, ARect.Top+(sMargin), String2);
      end
      else
      begin
        y := (ARect.Top + ARect.Bottom - TextHeight(fString)) div 2;
        if (y < 0) then y := 0;
          TextOut(x, y, fString);
      end;
      if (ArrowString <> '') then
      begin
        Font.Name := 'Marlett';
        Font.Charset := Default_CharSet;
        Font.Pitch := fpDefault;
        Font.Style := [];
        TextOut(ArrowX, y, ArrowString);
      end;
    end;
end;
procedure TPivotButton.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  sdimGroup, ddimGroup: TDimGroup;
  sIndex, dIndex: Integer;
begin
  inherited;
  Accept := False;
  if (FType = pbSummary) then Exit;
  if not (Source is TPivotButton) then Exit;
  if TPivotButton(Source).FSource <> FSource then Exit;
  ddimGroup := mydiminfo.iGroup;
  sdimGroup := TPivotButton(Source).MyDimInfo.IGroup;
  sIndex := TPivotButton(Source).MyDimInfo.IIndex;
  dIndex := MyDimInfo.IIndex;
  if (FType = pbTarget) or (X > (Width - (Width div 8))) then   { add to the right }
  begin
    if (ddimGroup = sdimGroup) and ((dIndex = (sIndex-1)) or (dIndex = sIndex)) then
      Exit;
    TPivotButton(Source).DragCursor := crDimIns;
  end
  else if (X < Width div 8) then
  begin
    if (ddimGroup = sdimGroup) and (dIndex = (sIndex + 1)) then Exit;
    TPivotButton(Source).DragCursor := crDimIns;
  end
  else        
  begin
    if (ddimGroup = sdimGroup) and (dIndex = sIndex) then
      TPivotButton(Source).DragCursor := crDimMove;
    TPivotButton(Source).DragCursor := crDimMove;
  end;
  Accept := True;
end;

procedure TPivotButton.DragDrop(Source: TObject; X, Y: Integer);
var
  sdimGroup, ddimGroup: TDimGroup;
  sIndex, dIndex: Integer;
begin
  inherited;
  ddimGroup := mydiminfo.iGroup;
  sdimGroup := TPivotButton(Source).MyDimInfo.IGroup;
  sIndex := TPivotButton(Source).MyDimInfo.iIndex;
  dIndex := MyDimInfo.iIndex;
  if (ddimGroup = sdimGroup) and (sIndex = dIndex) then
    Exit;  { do not drop on self }
  if (FType = pbTarget) or (X > (Width - (Width div 8))) then
    FSource.MoveDimIndexes(sdimGroup, ddimGroup, sIndex, dIndex+1, False)
  else if (X < (Width div 8)) then
    FSource.MoveDimIndexes(sdimGroup, ddimGroup, sIndex, dIndex, False)
  else
    FSource.SwapDimIndexes(sdimGroup, ddimGroup, sIndex, dIndex, False);
end;

procedure TQuickMenu.Clear;
begin
  while (Items.count > 0) do
    Items.Delete(0);
end;

procedure TQuickMenu.PopUpAtMe(aControl: TWinControl; x,y: Integer);
var
  aPoint: TPoint;
begin
  aPoint.x := x;
  aPoint.y := y;
  aPoint := aControl.ClientToScreen(aPoint);
  PopUp(aPoint.x, aPoint.y);
end;

procedure TQuickMenu.AddLine(const value: string; Prop: TMenuProp; Action: Integer);
var
  aMenuItem: TQuickMenuitem;
begin
  aMenuItem := TQuickMenuItem.Create(self);
  aMenuItem.Action := Action;
  aMenuItem.Caption := value;
  aMenuItem.Enabled := True;
  aMenuItem.OnClick := EHOnItemClick;
  if (Prop = tmChecked) then aMenuItem.Checked := True;
  if (Prop = tmRadio) then
  begin
    aMenuItem.Checked := True;
    aMenuItem.RadioItem := True;
  end;
  Items.Add(aMenuItem);
end;

procedure TQuickMenu.SetTitle(value: string);
begin
  Clear;
  AddLine(value, tmNone, -1);
  AddLine('-', tmNone, -1);
end;

procedure TQuickMenu.EHOnItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Items.count-1 do
  begin
    if (Sender = Items[i]) then
    begin
      FAction := TQuickMenuItem(Items[i]).Action;
      if (FAction >= 0) and assigned (FOnSelected) then
        FOnSelected(self);
      Exit;
    end;
  end;
end;

end.
