{   Component(s):
    tcyNavPanel

    Description:
    A panel that accept key stroke and visually paint frame on child controls
    The purpous is to simulate a multi-items screen options, navigating into them by stroke.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyNavPanel;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses VCL.cyClasses, VCL.cyTypes, cyAdvPanel, Windows, Graphics, classes, Messages, Controls, SysUtils;

type
  TNavigationOption = (noAutoFollowClickedControls,
                       noAutoFollowFocusedControls,
                       noNavigateWithArrowKeys
  );

  TNavigationOptions = Set of TNavigationOption;


  TProcCanNavigateToControl = procedure (Sender: TObject; ToControl: TControl; const KeyPressed: Word; var Accept: Boolean) of object;
  TProcDrawControlFrame = procedure (Sender: TObject; aControl: TControl; var Draw: Boolean) of object;
  TProcActiveControlMouseActivate = procedure (Sender: TObject; MouseActivatedControl: TControl) of object;
  TProcActiveControlFocused = procedure (Sender: TObject; FocusedControl: TControl) of object;

  TcyNavPanel = class(TcyAdvPanel)
  private
    FInactiveControlsFrame: TcyBevels;
    FActiveControlFrame: TcyBevels;
    FActiveControl: TControl;
    FCanNavigateToControl: TProcCanNavigateToControl;
    FOnDeactivateControl: TNotifyEvent;
    FOnActivateControl: TNotifyEvent;
    FOnDrawActiveControlFrame: TProcDrawControlFrame;
    FOnDrawInactiveControlFrame: TProcDrawControlFrame;
    FOptions: TNavigationOptions;
    FOnActiveControlMouseActivate: TProcActiveControlMouseActivate;
    FOnActiveControlFocused: TProcActiveControlFocused;
    procedure SetActiveControlFrame(const Value: TcyBevels);
    procedure SetInactiveControlsFrame(const Value: TcyBevels);
    procedure SetActiveControl(const Value: TControl);
    procedure ActiveControlFrameChange(Sender: TObject);
    procedure InactiveControlFrameChange(Sender: TObject);
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure SetOptions(const Value: TNavigationOptions);
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawBackground(const aRect: TRect); override;
    function  DoActivateControl(aControl: TControl): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawFrameControl(aControl: TControl; withBevels: TcyBevels);
    procedure Navigate(KeyPressed: Word);
  published
    property ActiveControl: TControl read FActiveControl write SetActiveControl;
    property ActiveControlFrame: TcyBevels read FActiveControlFrame write SetActiveControlFrame;
    property InactiveControlsFrame: TcyBevels read FInactiveControlsFrame write SetInactiveControlsFrame;
    property CanNavigateToControl: TProcCanNavigateToControl read FCanNavigateToControl write FCanNavigateToControl;
    property Options: TNavigationOptions read FOptions write SetOptions default [noAutoFollowClickedControls, noAutoFollowFocusedControls, noNavigateWithArrowKeys];
    property OnActivateControl: TNotifyEvent read FOnActivateControl write FOnActivateControl;
    property OnActiveControlMouseActivate: TProcActiveControlMouseActivate read FOnActiveControlMouseActivate write FOnActiveControlMouseActivate;
    property OnActiveControlFocused: TProcActiveControlFocused read FOnActiveControlFocused write FOnActiveControlFocused;
    property OnDeactivateControl: TNotifyEvent read FOnDeactivateControl write FOnDeactivateControl;
    property OnDrawActiveControlFrame: TProcDrawControlFrame read FOnDrawActiveControlFrame write FOnDrawActiveControlFrame;
    property OnDrawInactiveControlFrame: TProcDrawControlFrame read FOnDrawInactiveControlFrame write FOnDrawInactiveControlFrame;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

implementation

uses Types;

constructor TcyNavPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FActiveControl := Nil;
  FActiveControlFrame := TcyBevels.Create(self, TcyBevel);
  FInactiveControlsFrame := TcyBevels.Create(self, TcyBevel);
  FActiveControlFrame.OnChange := ActiveControlFrameChange;
  FInactiveControlsFrame.OnChange := InactiveControlFrameChange;
  FOptions := [noAutoFollowClickedControls, noAutoFollowFocusedControls, noNavigateWithArrowKeys];

  // Determine at design time if the form is loading or we have just added the component at design time :
  if csDesigning in ComponentState then
    if Owner <> nil then
      if not (csLoading in Owner.ComponentState) then // we have just added the component at design time
      begin
        TabStop := true;     // In order to navigate with arrows

        with FActiveControlFrame.Add do
        begin
          HighlightColor := clNavy;
          ShadowColor := clNavy;
          Width := 4;
        end;

        with FActiveControlFrame.Add do
        begin
          HighlightColor := clNavy;
          ShadowColor := clNavy;
          Width := 4;
          Style := bcTransparent;
        end;
      end;
end;

destructor TcyNavPanel.Destroy;
begin
  FActiveControlFrame.Free;
  FActiveControlFrame := Nil;
  FInactiveControlsFrame.Free;
  FInactiveControlsFrame := Nil;

  inherited Destroy;
end;

// Accept key press on component
procedure TcyNavPanel.WMGetDlgCode(var Msg: TMessage);
begin
  inherited;

  if TabStop then
    Msg.Result:= Msg.Result or DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

// When new TWinControl gains focus :
procedure TcyNavPanel.CMFocusChanged(var Message: TCMFocusChanged);
begin
  if noAutoFollowFocusedControls in FOptions then
    if Message.Sender <> Self.FActiveControl then
      if DoActivateControl(Message.Sender) then
      begin
        if Assigned(FOnActiveControlFocused) then
          FOnActiveControlFocused(Self, Message.Sender);
      end;

  inherited;
end;

procedure TcyNavPanel.WndProc(var Msg: TMessage);
var
  aControl: TControl;
  ScreenPos: TPoint;
  AllowDisabled: Boolean;
begin
  inherited WndProc(Msg);

(*
  if Msg.Msg = WM_LBUTTONUP then // Only works for TGraphicControls ...
  begin
    AllowDisabled := true;
    aControl := Self.ControlAtPos(SmallPointToPoint(TWMMouse(Msg).Pos), AllowDisabled, true);

    if Assigned(aControl) then
      if FActiveControl <> aControl then
        ActiveControl := aControl;
  end;                *)

  if Msg.Msg = WM_MOUSEACTIVATE then              // !!! WM_MOUSEACTIVATE fired for all components on the form !!!
    if noAutoFollowClickedControls in FOptions then
      if TWMMouseActivate(Msg).Result in [MA_ACTIVATE, MA_ACTIVATEANDEAT] then    // Activates a control ...
      begin
        // Determine control under mouse :
        if GetCursorPos(ScreenPos) then
        begin
          AllowDisabled := true;
          aControl := Self.ControlAtPos(Self.ScreenToClient(ScreenPos), AllowDisabled, true);  // Info: AllLevels parameter does not exists on Delphi 7 ...

          if Assigned(aControl) then
            if DoActivateControl(aControl) then
            begin
              if Assigned(FOnActiveControlMouseActivate) then
                FOnActiveControlMouseActivate(Self, aControl);
            end;
        end;
      end;
end;

procedure TcyNavPanel.DrawBackground(const aRect: TRect);
var
  c: Integer;
  Draw: Boolean;
begin
  inherited;

  // Draw controls frame :
  for c := 0 to Self.ControlCount-1 do
    if Self.Controls[c].Visible then
    begin
      Draw := true;

      if Self.Controls[c] = FActiveControl then
      begin
        if Assigned(FOnDrawActiveControlFrame) then
          FOnDrawActiveControlFrame(Self.Controls[c], Self.Controls[c], Draw);

        if Draw then
          DrawFrameControl(Self.Controls[c], FActiveControlFrame);
      end
      else begin
        if Assigned(FOnDrawInactiveControlFrame) then
          FOnDrawInactiveControlFrame(Self.Controls[c], Self.Controls[c], Draw);

        if Draw then
          DrawFrameControl(Self.Controls[c], FInactiveControlsFrame);
      end;
    end;
end;

procedure TcyNavPanel.DrawFrameControl(aControl: TControl; withBevels: TcyBevels);
var
  ControlRect: TRect;
  BevelsWidth: Integer;
begin
  ControlRect := aControl.BoundsRect;
  BevelsWidth := withBevels.BevelsWidth;
  InflateRect(ControlRect, BevelsWidth, BevelsWidth);

  withBevels.DrawBevels(Canvas, ControlRect, false);
end;

procedure TcyNavPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  // Navigation handling :
  if noNavigateWithArrowKeys in FOptions then
    if Key in [VK_UP, VK_DOWN, VK_RIGHT, VK_LEFT] then
      Navigate(Key);
end;

procedure TcyNavPanel.Navigate(KeyPressed: Word);
var
  Nearest: TControl;
  NearestDistance_Up, NearestDistance_Right, NearestDistance_Down, NearestDistance_Left: Extended;
  c: Integer;
  ControlAccepted: Boolean;

        function CalcDistance(fromPoint, toPoint: TPoint): Extended;
        var w, h: Integer;
        begin
          w := sqr(fromPoint.X - toPoint.X);  // sqr = Carré
          h := sqr(fromPoint.Y - toPoint.Y);
          Result := sqrt(w + h);              // sqrt = Racine carrée
        end;

        function CalcDistance_Up(fromControl, ToControl: TControl): Extended;
        begin
          if ToControl.Top + ToControl.Height > fromControl.Top then
            Result := Self.Height
          else
            if ToControl.Left + ToControl.Width < fromControl.Left then  // ToControl at left
              Result := CalcDistance( Point(ToControl.Left + ToControl.Width, ToControl.Top + ToControl.Height), Point(fromControl.Left, fromControl.Top) )
            else
              if ToControl.Left > fromControl.Left + fromControl.Width then  // ToControl at right
                Result := CalcDistance( Point(ToControl.Left, ToControl.Top + ToControl.Height), Point(fromControl.Left + fromControl.Width, fromControl.Top) )
              else
                Result := fromControl.Top - (ToControl.Top + ToControl.Height);
        end;

        function CalcDistance_Left(fromControl, ToControl: TControl): Extended;
        begin
          if ToControl.Left + ToControl.Width > fromControl.Left then
            Result := Self.Width
          else
            if ToControl.Top + ToControl.Height < fromControl.Top then  // ToControl at top
              Result := CalcDistance( Point(ToControl.Top + ToControl.Height, ToControl.Left + ToControl.Width), Point(fromControl.Top, fromControl.Left) )
            else
              if ToControl.Top > fromControl.Top + fromControl.Height then  // ToControl at bottom
                Result := CalcDistance( Point(ToControl.Top, ToControl.Left + ToControl.Width), Point(fromControl.Top + fromControl.Height, fromControl.Left) )
              else
                Result := fromControl.Left - (ToControl.Left + ToControl.Width);
        end;

        function CalcDistance_Down(fromControl, ToControl: TControl): Extended;
        begin
          if fromControl.Top + fromControl.Height > toControl.Top then
            Result := Self.Height
          else
            if ToControl.Left + ToControl.Width < fromControl.Left then  // ToControl at left
              Result := CalcDistance( Point(fromControl.Left + fromControl.Width, fromControl.Top + fromControl.Height), Point(toControl.Left, toControl.Top) )
            else
              if ToControl.Left > fromControl.Left + fromControl.Width then  // ToControl at right
                Result := CalcDistance( Point(fromControl.Left, fromControl.Top + fromControl.Height), Point(toControl.Left + toControl.Width, toControl.Top) )
              else
                Result := toControl.Top - (fromControl.Top + fromControl.Height);
        end;

        function CalcDistance_Right(fromControl, ToControl: TControl): Extended;
        begin
          if fromControl.Left + fromControl.Width > toControl.Left then
            Result := Self.Width
          else
            if ToControl.Top + ToControl.Height < fromControl.Top then  // ToControl at bottom
              Result := CalcDistance( Point(fromControl.Top + fromControl.Height, fromControl.Left + fromControl.Width), Point(toControl.Top, toControl.Left) )
            else
              if ToControl.Top > fromControl.Top + fromControl.Height then  // ToControl at top
                Result := CalcDistance( Point(fromControl.Top, fromControl.Left + fromControl.Width), Point(toControl.Top + toControl.Height, toControl.Left) )
              else
                Result := toControl.Left - (fromControl.Left + fromControl.Width);
        end;

        procedure SetNearest(withControl: TControl);
        begin
          Nearest := withControl;

          if (Nearest <> Nil) and (FActiveControl <> Nil) then
          begin
            NearestDistance_Up    := CalcDistance_Up(FActiveControl, Nearest);
            NearestDistance_Right := CalcDistance_Right(FActiveControl, Nearest);
            NearestDistance_Down  := CalcDistance_Down(FActiveControl, Nearest);
            NearestDistance_Left  := CalcDistance_Left(FActiveControl, Nearest);
          end;
        end;


begin
  Nearest := Nil;

  if not Assigned(FActiveControl) then
  begin
    if Self.ControlCount > 0 then
    begin
      ControlAccepted := true;

      if Assigned(FCanNavigateToControl) then
        FCanNavigateToControl(Self, Self.Controls[0], KeyPressed, ControlAccepted);

      if ControlAccepted then
        SetNearest(Self.Controls[0]);
    end;
  end
  else
    with FActiveControl do
    begin
      for c := 0 to Self.ControlCount-1 do
      begin
        ControlAccepted := true;

        if KeyPressed = VK_UP then
        begin
          if (Self.Controls[c].Visible) and (Self.Controls[c].BoundsRect.Top < BoundsRect.Top)
          then
            if Nearest = Nil then
            begin
              if Assigned(FCanNavigateToControl) then
                FCanNavigateToControl(Self, Self.Controls[c], KeyPressed, ControlAccepted);

              if ControlAccepted then
                SetNearest(Self.Controls[c]);
            end
            else
              if (CalcDistance_Up(FActiveControl, Self.Controls[c]) < NearestDistance_Up)
              then begin
                if Assigned(FCanNavigateToControl) then
                  FCanNavigateToControl(Self, Self.Controls[c], KeyPressed, ControlAccepted);

                if ControlAccepted then
                  SetNearest(Self.Controls[c]);
              end;
        end;

        if KeyPressed = VK_RIGHT then
        begin
          if (Self.Controls[c].Visible) and (Self.Controls[c].BoundsRect.Left > BoundsRect.Left)
          then
            if Nearest = Nil then
            begin
              if Assigned(FCanNavigateToControl) then
                FCanNavigateToControl(Self, Self.Controls[c], KeyPressed, ControlAccepted);

              if ControlAccepted then
                SetNearest(Self.Controls[c]);
            end
            else
              if (CalcDistance_Right(FActiveControl, Self.Controls[c]) < NearestDistance_Right)
              then begin
                if Assigned(FCanNavigateToControl) then
                  FCanNavigateToControl(Self, Self.Controls[c], KeyPressed, ControlAccepted);

                if ControlAccepted then
                  SetNearest(Self.Controls[c]);
              end;
        end;

        if KeyPressed = VK_DOWN then
        begin
          if (Self.Controls[c].Visible) and (Self.Controls[c].BoundsRect.Top > BoundsRect.Top)
          then
            if Nearest = Nil then
            begin
              if Assigned(FCanNavigateToControl) then
                FCanNavigateToControl(Self, Self.Controls[c], KeyPressed, ControlAccepted);

              if ControlAccepted then
                SetNearest(Self.Controls[c]);
            end
            else
              if (CalcDistance_Down(FActiveControl, Self.Controls[c]) < NearestDistance_Down)
(* TESTING
   Le fait que l' utilisateur aurait mieux fait d'appuyer su une autre touche pour aller sur le même controle n'optimise pas du tout la navigation car du coup,
   il va se positionner sur un control bien plus loin ...

               or (CalcDistance_Left(FActiveControl, Self.Controls[c]) < NearestDistance_Down)            // Shortest distance pressing Left to go to Self.Controls[c]
                or (CalcDistance_Right(FActiveControl, Self.Controls[c]) < NearestDistance_Down)          // Shortest distance pressing Right to go to Self.Controls[c]  *)
              then begin
                if Assigned(FCanNavigateToControl) then
                  FCanNavigateToControl(Self, Self.Controls[c], KeyPressed, ControlAccepted);

                if ControlAccepted then
                  SetNearest(Self.Controls[c]);
              end
        end;

        if KeyPressed = VK_LEFT then
        begin
          if (Self.Controls[c].Visible) and (Self.Controls[c].BoundsRect.Left < BoundsRect.Left)
          then
            if Nearest = Nil then
            begin
              if Assigned(FCanNavigateToControl) then
                FCanNavigateToControl(Self, Self.Controls[c], KeyPressed, ControlAccepted);

              if ControlAccepted then
                SetNearest(Self.Controls[c]);
            end
            else
              if (CalcDistance_Left(FActiveControl, Self.Controls[c]) < NearestDistance_Left)
              then begin
                if Assigned(FCanNavigateToControl) then
                  FCanNavigateToControl(Self, Self.Controls[c], KeyPressed, ControlAccepted);

                if ControlAccepted then
                  SetNearest(Self.Controls[c]);
              end;
        end;
      end;
    end;

  // Select nearest control :
  if Nearest <> Nil then
    ActiveControl := Nearest;
end;

procedure TcyNavPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//  if CanFocus then
//    SetFocus;

  inherited;
end;

procedure TcyNavPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if FActiveControl <> nil then
    if (Operation = opRemove) and (AComponent = FActiveControl) then
      FActiveControl := nil;
end;

function TcyNavPanel.DoActivateControl(aControl: TControl): Boolean;
var
  NewActiveControl: TControl;
  ControlAccepted: Boolean;
begin
  Result := false;
  NewActiveControl := Nil;

  while aControl <> Nil do
  begin
    if aControl.Parent = Self then
    begin
      NewActiveControl := aControl;
      Break;
    end;

    aControl := aControl.Parent;
  end;

  if NewActiveControl <> Nil then
    if NewActiveControl <> ActiveControl then
    begin
      ControlAccepted := true;

      if Assigned(FCanNavigateToControl) then
        FCanNavigateToControl(Self, aControl, 0, ControlAccepted);

      if ControlAccepted then
      begin
        ActiveControl := aControl;
        Result := true;
      end;
    end;
end;

procedure TcyNavPanel.SetActiveControl(const Value: TControl);
begin
  if FActiveControl <> Value then
  begin
    if Assigned(FOnDeactivateControl) then
      FOnDeactivateControl(Self);

    FActiveControl := Value;

    if Value <> nil then
      Value.FreeNotification(Self);  // Inform if component removed ...

    if Assigned(FOnActivateControl) then
      FOnActivateControl(Self);

    Invalidate;
  end;
end;

procedure TcyNavPanel.SetActiveControlFrame(const Value: TcyBevels);
begin
  FActiveControlFrame := Value;
end;

procedure TcyNavPanel.SetInactiveControlsFrame(const Value: TcyBevels);
begin
  FInactiveControlsFrame := Value;
end;

procedure TcyNavPanel.SetOptions(const Value: TNavigationOptions);
begin
  FOptions := Value;
end;

procedure TcyNavPanel.ActiveControlFrameChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyNavPanel.InactiveControlFrameChange(Sender: TObject);
begin
  Invalidate;
end;

end.
