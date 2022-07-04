unit KAHRollForm;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, ExtCtrls;

Const
  BevelCount = 5;
type
  TRollType    = (rtLeft, rtRight, rtTop, rtBottom);
  TKABevel     = class(TBevel)
  protected
    procedure   DblClick; override;
  End;

  TKAHRollForm = class(TCustomPanel)
  private
    { Private declarations }
    F_Bevels        : Array[1..BevelCount] of TBevel;
    F_ExpandedWidth : Integer;
    F_ShrinkedWidth : Integer;
    F_BevelWidth    : Integer;
    F_Controls      : TList;
    F_RollType      : TRollType;
    F_RollAtStartup : Boolean;
    F_RollAnimation : Boolean;
    F_RollStep      : Integer;
    F_RollControls  : Boolean;
    Procedure         F_Set_RollType(Value : TRollType);
  protected
    { Protected declarations }
    Function    IsMyBevel(Ctrl:TControl):Boolean;
    Procedure   HideAll;
    Procedure   ShowAll;
    Procedure   MoveAllToTheLeft;
    Procedure   MoveAllToTheRight;
    Procedure   MoveAllToTheTop;
    Procedure   MoveAllToTheBottom;

    Procedure   MoveAllToTheLeftByStep(Step_:Integer);
    Procedure   MoveAllToTheRightByStep(Step_:Integer);
    Procedure   MoveAllToTheTopByStep(Step_:Integer);
    Procedure   MoveAllToTheBottomByStep(Step_:Integer);

    Procedure   HorizontalAnimeRoll(From_, To_, Step : Integer);
    Procedure   VerticalAnimeRoll(From_, To_, Step : Integer);
    Procedure   Loaded; override;
    Procedure   DoublClick;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;

    property    Align;
    property    Alignment;
    {$IFDEF D4UP}
    property    Anchors;
    property    AutoSize;
    {$ENDIF}
    property    BevelInner;
    property    BevelOuter;
    property    BevelWidth;
    {$IFDEF D4UP}
    property    BiDiMode;
    {$ENDIF}
    property    BorderWidth;
    property    BorderStyle;
    property    Caption;
  published
    { Published declarations }
    Property RollType      : TRollType Read F_RollType      Write F_Set_RollType;
    Property RollAtStartup : Boolean   Read F_RollAtStartup Write F_RollAtStartup;
    Property RollAnimation : Boolean   Read F_RollAnimation Write F_RollAnimation;
    Property RollStep      : Integer   Read F_RollStep      Write F_RollStep;
    Property RollControls  : Boolean   Read F_RollControls  Write F_RollControls;
    property Color;
    {$IFDEF D4UP}
    property Constraints;
    {$ENDIF}
    property Ctl3D;
    {$IFDEF D4UP}
    property UseDockManager;
    property DockSite;
    {$ENDIF}
    property DragCursor;
    {$IFDEF D4UP}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    {$IFDEF D4UP}
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF D4UP}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    {$IFDEF D4UP}
    property OnConstrainedResize;
    {$ENDIF}
    {$IFDEF D5UP}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF D4UP}
    property OnDockDrop;
    property OnDockOver;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF D4UP}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF D4UP}
    property OnGetSiteInfo;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF D4UP}
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;
    {$IFDEF D4UP}
    property OnUnDock;
    {$ENDIF}
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKAHRollForm]);
end;

{ TKAHRollForm }

procedure TKABevel.DblClick;
Begin
  if Owner is TKAHRollForm Then TKAHRollForm(Owner).DoublClick;
End;

constructor TKAHRollForm.Create(AOwner: TComponent);
Var
  X : Integer;
begin
  inherited Create(AOwner);
  ControlStyle        := ControlStyle - [csSetCaption];
  ControlStyle        := ControlStyle - [csFramed];
  F_RollType          := rtRight;
  F_RollAtStartup     := True;
  F_RollAnimation     := False;
  F_RollControls      := True;
  F_RollStep          := 2;
  Align               := alRight;
  F_Controls          := TList.Create;
  F_ShrinkedWidth     := GetSystemMetrics(SM_CXSIZE);;
  F_BevelWidth        := F_ShrinkedWidth Div BevelCount;
  For X := 1 To BevelCount do
      Begin
        F_Bevels[X]        := TKABevel.Create(Self);
        F_Bevels[X].Style  := bsRaised;
        F_Bevels[X].Align  := AlRight;
        F_Bevels[X].Width  := F_BevelWidth;
        F_Bevels[X].Parent := Self;
      End;
  Self.BevelInner  := bvNone;
  Self.BevelOuter  := bvNone;
  Self.FullRepaint := False;
end;

destructor TKAHRollForm.Destroy;
begin
  F_Controls.Free;
  inherited Destroy;
end;

Function TKAHRollForm.IsMyBevel(Ctrl:TControl):Boolean;
Var
 X : Integer;
Begin
 Result := True;
 For X  := 1 to BevelCount do If F_Bevels[X]=Ctrl Then Exit;
 Result := False;
End;

procedure TKAHRollForm.HideAll;
Var
  X : Integer;
begin
  F_Controls.Clear;
  For X := 0 To Self.ControlCount-1 do
      Begin
       if Self.Controls[X].Visible Then
          Begin
            if Not IsMyBevel(Self.Controls[X]) Then
               Begin
                 F_Controls.Add(Self.Controls[X]);
                 Self.Controls[X].Visible:= False;
               End;
          End;
      End;
end;

procedure TKAHRollForm.ShowAll;
Var
  X : Integer;
begin
  For X := 0 To F_Controls.Count-1 do
      Begin
        TControl(F_Controls.Items[X]).Visible := True;
      End;
end;

procedure TKAHRollForm.MoveAllToTheLeft;
Var
  X : Integer;
Begin
 if Owner is TForm Then
     Begin
       For X := 0 To TForm(Owner).ControlCount-1 do
           Begin
            if TForm(Owner).Controls[X] <> Self Then
               TForm(Owner).Controls[X].Left := TForm(Owner).Controls[X].Left-(F_ExpandedWidth-F_ShrinkedWidth);
           End;
     End;
End;

procedure TKAHRollForm.MoveAllToTheRight;
Var
  X : Integer;
Begin
 if Owner is TForm Then
     Begin
       For X := 0 To TForm(Owner).ControlCount-1 do
           Begin
            if TForm(Owner).Controls[X] <> Self Then
               TForm(Owner).Controls[X].Left := TForm(Owner).Controls[X].Left+(F_ExpandedWidth-F_ShrinkedWidth);
           End;
     End;
End;

procedure TKAHRollForm.MoveAllToTheTop;
Var
  X : Integer;
Begin
 if Owner is TForm Then
     Begin
       For X := 0 To TForm(Owner).ControlCount-1 do
           Begin
            if TForm(Owner).Controls[X] <> Self Then
               TForm(Owner).Controls[X].Top := TForm(Owner).Controls[X].Top-(F_ExpandedWidth-F_ShrinkedWidth);
           End;
     End;
End;

procedure TKAHRollForm.MoveAllToTheBottom;
Var
  X : Integer;
Begin
 if Owner is TForm Then
     Begin
       For X := 0 To TForm(Owner).ControlCount-1 do
           Begin
            if TForm(Owner).Controls[X] <> Self Then
               TForm(Owner).Controls[X].Top := TForm(Owner).Controls[X].Top+(F_ExpandedWidth-F_ShrinkedWidth);
           End;
     End;
End;


procedure TKAHRollForm.MoveAllToTheLeftByStep(Step_:Integer);
Var
  X : Integer;
Begin
 if Owner is TForm Then
     Begin
       For X := 0 To TForm(Owner).ControlCount-1 do
           Begin
            if TForm(Owner).Controls[X] <> Self Then
               TForm(Owner).Controls[X].Left := TForm(Owner).Controls[X].Left-Step_;
           End;
     End;
End;

procedure TKAHRollForm.MoveAllToTheRightByStep(Step_:Integer);
Var
  X : Integer;
Begin
 if Owner is TForm Then
     Begin
       For X := 0 To TForm(Owner).ControlCount-1 do
           Begin
            if TForm(Owner).Controls[X] <> Self Then
               TForm(Owner).Controls[X].Left := TForm(Owner).Controls[X].Left+Step_;
           End;
     End;
End;

procedure TKAHRollForm.MoveAllToTheTopByStep(Step_:Integer);
Var
  X : Integer;
Begin
 if Owner is TForm Then
     Begin
       For X := 0 To TForm(Owner).ControlCount-1 do
           Begin
            if TForm(Owner).Controls[X] <> Self Then
               TForm(Owner).Controls[X].Top := TForm(Owner).Controls[X].Top-Step_;
           End;
     End;
End;

procedure TKAHRollForm.MoveAllToTheBottomByStep(Step_:Integer);
Var
  X : Integer;
Begin
 if Owner is TForm Then
     Begin
       For X := 0 To TForm(Owner).ControlCount-1 do
           Begin
            if TForm(Owner).Controls[X] <> Self Then
               TForm(Owner).Controls[X].Top := TForm(Owner).Controls[X].Top+Step_;
           End;
     End;
End;

Procedure TKAHRollForm.Loaded;
Begin
  Inherited Loaded;
  if csDesigning in ComponentState Then Exit;
  if Owner is TForm Then
     Begin
       if RollAtStartup Then HideAll;
       if F_RollType=rtLeft Then
          Begin
            F_ExpandedWidth    := Width;
            if RollAtStartup Then
               Begin
                 TForm(Owner).Width := TForm(Owner).Width-(Self.Width-F_ShrinkedWidth);
                 Width              := F_ShrinkedWidth;
                 MoveAllToTheLeft;
               End;
          End
       Else
       if F_RollType=rtRight Then
          Begin
            F_ExpandedWidth    := Width;
            if RollAtStartup Then
               Begin
                 TForm(Owner).Width := TForm(Owner).Width-(Self.Width-F_ShrinkedWidth);
                 Width              := F_ShrinkedWidth;
               End;
          End
       Else
       if F_RollType=rtTop Then
          Begin
            F_ExpandedWidth     := Height;
            if RollAtStartup Then
               Begin
                 TForm(Owner).Height := TForm(Owner).Height-(Self.Height-F_ShrinkedWidth);
                 Height              := F_ShrinkedWidth;
                 MoveAllToTheTop;
               End;
          End
       Else
       if F_RollType=rtBottom Then
          Begin
            F_ExpandedWidth     := Height;
            if RollAtStartup Then
               Begin
                 TForm(Owner).Height := TForm(Owner).Height-(Self.Height-F_ShrinkedWidth);
                 Height              := F_ShrinkedWidth;
               End;
          End;
     End;
End;

procedure TKAHRollForm.F_Set_RollType(Value: TRollType);
Var
  X : Integer;
begin
 if F_RollType <> Value Then
    Begin
      F_RollType := Value;
      if F_RollType=rtLeft Then
         Begin
           Align := alLeft;
           For X := 1 To BevelCount do
               Begin
                 F_Bevels[X].Align  := AlLeft;
               End;
         End
      Else
      if F_RollType=rtRight Then
         Begin
           Align := alRight;
           For X := 1 To BevelCount do
               Begin
                 F_Bevels[X].Align  := AlRight;
               End;
         End
      Else
      if F_RollType=rtTop Then
         Begin
           Align := alTop;
           For X := 1 To BevelCount do
               Begin
                 F_Bevels[X].Align  := AlTop;
               End;
         End
      Else
      if F_RollType=rtBottom Then
         Begin
           Align := alBottom;
           For X := 1 To BevelCount do
               Begin
                 F_Bevels[X].Align  := AlBottom;
               End;
         End;
    End;
end;

Procedure TKAHRollForm.HorizontalAnimeRoll(From_, To_, Step : Integer);
Var
 X : Integer;
Begin
  X     := (To_   Div Step)*Step;
  From_ := (From_ Div Step)*Step;
  Repeat
    TForm(Owner).Width := TForm(Owner).Width+Step;
    if F_RollControls Then
       Begin
         Width := Width+Step;
         If F_RollType=rtLeft Then
            Begin
              if Step > 0 Then
                 MoveAllToTheRightByStep(Step)
               Else
                 MoveAllToTheLeftByStep(-Step);
            End;
       End;
    From_ := From_+Step;
    Application.ProcessMessages;
  Until From_=X;
End;


Procedure TKAHRollForm.VerticalAnimeRoll(From_, To_, Step : Integer);
Var
 X  : Integer;
Begin
  X     := (To_   Div Step)*Step;
  From_ := (From_ Div Step)*Step;
  Repeat
    TForm(Owner).Height := TForm(Owner).Height+Step;
    if F_RollControls Then
       Begin
         Height := Height+Step;
         If F_RollType=rtTop Then
            Begin
              if Step > 0 Then
                 MoveAllToTheBottomByStep(Step)
               Else
                 MoveAllToTheTopByStep(-Step);
            End;
       End;
    From_ := From_+Step;
    Application.ProcessMessages;
  Until From_=X;
End;


procedure TKAHRollForm.DoublClick;
Var
 Temp : Integer;
begin
  if Not F_RollAnimation Then F_RollControls := False;
  if Owner is TForm Then
     Begin
      if F_RollType=rtLeft Then
         Begin
           if Width=F_ShrinkedWidth Then
              Begin
                Temp               := TForm(Owner).Width;
                if F_RollControls  Then ShowAll;
                If F_RollAnimation Then
                   HorizontalAnimeRoll(TForm(Owner).Width,TForm(Owner).Width+(F_ExpandedWidth-F_ShrinkedWidth),F_RollStep);
                TForm(Owner).Width := Temp+(F_ExpandedWidth-F_ShrinkedWidth);
                Width              := F_ExpandedWidth;
                if NOT F_RollControls  Then
                   Begin
                     MoveAllToTheRight;
                     ShowAll;
                   End;
              End
           Else
              Begin
                Temp               := TForm(Owner).Width;
                if NOT F_RollControls Then
                   Begin
                     HideAll;
                     Width              := F_ShrinkedWidth;
                   End;
                If F_RollAnimation Then
                   HorizontalAnimeRoll(TForm(Owner).Width,TForm(Owner).Width-(F_ExpandedWidth-F_ShrinkedWidth),-F_RollStep);
                TForm(Owner).Width := Temp-(F_ExpandedWidth-F_ShrinkedWidth);
                if NOT F_RollControls Then MoveAllToTheLeft;
                if F_RollControls Then
                   Begin
                     HideAll;
                     Width              := F_ShrinkedWidth;
                   End;
              End;
         End
      Else
      if F_RollType=rtRight Then
         Begin
           if Width=F_ShrinkedWidth Then
              Begin
                Temp               := TForm(Owner).Width;
                if F_RollControls  Then ShowAll;
                If F_RollAnimation Then
                   HorizontalAnimeRoll(TForm(Owner).Width,TForm(Owner).Width+(F_ExpandedWidth-F_ShrinkedWidth),F_RollStep);
                TForm(Owner).Width := Temp+(F_ExpandedWidth-F_ShrinkedWidth);
                Width              := F_ExpandedWidth;
                if NOT F_RollControls  Then ShowAll;
              End
           Else
              Begin
                Temp               := TForm(Owner).Width;
                if NOT F_RollControls Then
                   Begin
                     HideAll;
                     Width              := F_ShrinkedWidth;
                   End;
                If F_RollAnimation Then
                   HorizontalAnimeRoll(TForm(Owner).Width,TForm(Owner).Width-(F_ExpandedWidth-F_ShrinkedWidth),-F_RollStep);
                TForm(Owner).Width := Temp-(F_ExpandedWidth-F_ShrinkedWidth);
                if F_RollControls Then
                   Begin
                     HideAll;
                     Width              := F_ShrinkedWidth;
                   End;
              End;
         End
      Else
      if F_RollType=rtTop Then
         Begin
           if Height=F_ShrinkedWidth Then
              Begin
                Temp                := TForm(Owner).Height;
                if F_RollControls  Then ShowAll;
                If F_RollAnimation Then
                   VerticalAnimeRoll( TForm(Owner).Height, TForm(Owner).Height+(F_ExpandedWidth-F_ShrinkedWidth),F_RollStep);
                TForm(Owner).Height := Temp+(F_ExpandedWidth-F_ShrinkedWidth);
                if NOT F_RollControls Then MoveAllToTheBottom;
                Height              := F_ExpandedWidth;
                if NOT F_RollControls Then ShowAll;
              End
           Else
              Begin
                Temp                := TForm(Owner).Height;
                if NOT F_RollControls Then
                   Begin
                     HideAll;
                     Height              := F_ShrinkedWidth;
                   End;
                If F_RollAnimation Then
                   VerticalAnimeRoll( TForm(Owner).Height, TForm(Owner).Height-(F_ExpandedWidth-F_ShrinkedWidth),-F_RollStep);
                TForm(Owner).Height := Temp-(F_ExpandedWidth-F_ShrinkedWidth);
                if NOT F_RollControls Then MoveAllToTheTop;
                if F_RollControls Then
                   Begin
                     HideAll;
                     Height              := F_ShrinkedWidth;
                   End;
              End;
         End
      Else
      if F_RollType=rtBottom Then
         Begin
           if Height=F_ShrinkedWidth Then
              Begin
                Temp                := TForm(Owner).Height;
                if F_RollControls Then ShowAll;
                If F_RollAnimation Then
                   VerticalAnimeRoll( TForm(Owner).Height, TForm(Owner).Height+(F_ExpandedWidth-F_ShrinkedWidth),F_RollStep);
                TForm(Owner).Height := Temp+(F_ExpandedWidth-F_ShrinkedWidth);
                Height              := F_ExpandedWidth;
                if NOT F_RollControls Then ShowAll;
              End
           Else
              Begin
                Temp                := TForm(Owner).Height;
                if NOT F_RollControls Then
                   Begin
                     HideAll;
                     Height              := F_ShrinkedWidth;
                   End;
                If F_RollAnimation Then
                   VerticalAnimeRoll( TForm(Owner).Height, TForm(Owner).Height-(F_ExpandedWidth-F_ShrinkedWidth),-F_RollStep);
                TForm(Owner).Height := Temp-(F_ExpandedWidth-F_ShrinkedWidth);
                if F_RollControls Then
                   Begin
                     HideAll;
                     Height              := F_ShrinkedWidth;
                   End;
              End;
         End;
     End;
end;



end.
