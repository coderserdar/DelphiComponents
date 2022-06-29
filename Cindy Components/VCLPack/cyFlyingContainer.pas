{   Component(s):
    tcyFlyingContainer

    Description:
    Create a form in popup style with any control inside until it loose focus.

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

unit cyFlyingContainer;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Forms, Windows, Controls, Types, cyBaseContainer;

type
  TcyFlyingContainer = class(TcyBaseContainer)
  private
    FCloseOnEscKey: Boolean;
    FCloseOnTabKey: Boolean;
    FCloseOnExit: Boolean;
  protected
    procedure DoFormDeactivate; override;
    procedure DoFormClose; override;
    procedure DoFormKeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute(const ScreenCoord: TPoint; const RestoreLastControlSize: Boolean = false; const RestoreLastControlPosition: Boolean = false); override;
    procedure Close; override;
    property Active;
    property FormContainer;
    property LastControlCoords;
  published
    property BorderStyle;
    property BorderIcons;
    property Caption;
    property Control;
    property DontActivate;
    property ShowHint;
    {$IFDEF UNICODE}
    property AlphaBlend;
    property AlphaBlendValue;
    property TransparentColor;
    property TransparentColorValue;
    {$ENDIF}
    property OnClose;
    property OnDeactivate;
    property OnKeyPress;
    property OnShow;
    property OnResize;
    property EnterKeyAction;
    property CloseOnEscKey: Boolean read FCloseOnEscKey write FCloseOnEscKey default true;
    property CloseOnTabKey: Boolean read FCloseOnTabKey write FCloseOnTabKey default true;
    property CloseOnExit: Boolean read FCloseOnExit write FCloseOnExit default true;
    procedure ExecuteAsSplashScreen;
  end;

function ShowStayOnTopControl(aControl: TControl; BS: TFormBorderStyle; BI: TBorderIcons; WS: TWindowState; Title: String; BeforeShowModal: TNotifyEvent): TForm;

implementation

function ShowStayOnTopControl(aControl: TControl; BS: TFormBorderStyle; BI: TBorderIcons; WS: TWindowState; Title: String; BeforeShowModal: TNotifyEvent): TForm;
var
  FrmStayOnTop: TForm;
begin
  Application.CreateForm(TForm, FrmStayOnTop);
  Result := FrmStayOnTop;

  with FrmStayOnTop do
  begin
    Caption      := Title;
    BorderIcons := BI; BorderStyle := Bs; WindowState := WS; FormStyle := FsStayOnTop;
    KeyPreview := True; Position  := PoScreenCenter;
    ClientHeight := aControl.Height; ClientWidth  := aControl.Width;
  end;

  aControl.Parent := FrmStayOnTop; aControl.Top := 0; aControl.Left := 0;

  if Assigned(BeforeShowModal) then
    BeforeShowModal(FrmStayOnTop);

  FrmStayOnTop.Show;
  FrmStayOnTop.Update;
end;

{TcyFlyingContainer}
constructor TcyFlyingContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCloseOnEscKey := true;
  FCloseOnTabKey := true;
  FCloseOnExit := true;
end;

procedure TcyFlyingContainer.DoFormClose;
begin
  inherited;
  FormContainer.Release;
end;

procedure TcyFlyingContainer.DoFormDeactivate;
begin
  inherited;
  if FormContainer.Visible and FCloseOnExit then
    FormContainer.Close;
end;

procedure TcyFlyingContainer.Execute(const ScreenCoord: TPoint; const RestoreLastControlSize: Boolean = false; const RestoreLastControlPosition: Boolean = false);
begin
  if (not Active) and (Control <> nil) then
  begin
    CreateFormContainer;
    SetContainerProperties;

    SaveControlProperties;

    if RestoreLastControlSize and ValidLastControlCoords then
    begin
      Control.Width := LastControlCoords.Right - LastControlCoords.Left;
      Control.Height := LastControlCoords.Bottom - LastControlCoords.Top;
    end;

    FormContainer.Position := poDesigned;

    if RestoreLastControlPosition and ValidLastControlCoords then
    begin
      FormContainer.Top := LastControlCoords.Top;
      FormContainer.Left := LastControlCoords.Left;
    end
    else begin
      FormContainer.Top := ScreenCoord.Y;
      FormContainer.Left := ScreenCoord.X;
    end;

    FormContainer.ClientWidth := Control.Width;
    FormContainer.ClientHeight := Control.Height;

    Control.Parent := FormContainer;
    Control.Align := alClient;
    Control.Visible := true;

    // Don' t show form shadow on owner's form with bsNone :
    // FormContainer.Show;
    FormContainer.Visible := true;    // makes shadow visible with bsNone ...

    // Not needed! FormContainer.SetFocus;
  end;
end;

procedure TcyFlyingContainer.ExecuteAsSplashScreen;
begin
  if (not Active) and (Control <> nil)
  then begin
    CreateFormContainer;
    SetContainerProperties;
    FormContainer.Position := poScreenCenter;
    FormContainer.FormStyle := fsStayOnTop;
    FormContainer.OnDeactivate := Nil;
    FormContainer.OnKeyPress := Nil;
    FormContainer.ClientWidth := Control.Width;
    FormContainer.ClientHeight := Control.Height;

    // Change control properties:
    SaveControlProperties;
    Control.Parent := FormContainer;
    Control.Align := alClient;
    Control.Visible := true;

    // Show form:
    FormContainer.Show;
    FormContainer.SetFocus;
    FormContainer.Update;
  end;
end;

// Called by you :
procedure TcyFlyingContainer.Close;
begin
  if Active then
    FormContainer.Close;
end;

procedure TcyFlyingContainer.DoFormKeyPress(var Key: Char);
begin
  Inherited;   // Call FOnKeyPress

  if (Key = #27) and (FCloseOnEscKey) then
  begin
    Key := #0;
    FormContainer.Close;
  end;

  if (Key = #9) and (FCloseOnTabKey) then
  begin
    Key := #0;
    FormContainer.Close;
  end;
end;

end.
