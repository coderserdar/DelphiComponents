unit CHEditBtn;

{ ##############################################################################
  TCHEditBtn

  Version   		:   1.0.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 04.04.2004    - First Release
  1.0.1 - 09.01.2005    - FreeNotification for dialogs

  ############################################################################ }

interface

uses
  Forms, Windows, SysUtils, Classes, Controls, StdCtrls, Graphics, Messages, ComCtrls,
  Dialogs, StrUtils, CHEdit, Buttons, CHFolderDialog, CHDateDialog;

type
  TEditModus = (emUser, emDate, emFilename, emDirectory);


type
  TCHEditBtn = class;

  TEditStyle = class(TPersistent)
  private
    FOwner : TCHEditBtn;
    FDateDialog: TCHDateDialog;
    FFolderDialog: TCHFolderDialog;
    FEditModus: TEditModus;
    FFileDialog: TOpenDialog;
    procedure DateClick(Sender: TObject);
    procedure DirectoryClick(Sender: TObject);
    procedure FileClick(Sender: TObject);
    procedure UserClick(Sender: TObject);
    procedure SetEditModus(const Value: TEditModus);
    procedure SetDateDialog(const Value: TCHDateDialog);
    procedure SetFileDialog(const Value: TOpenDialog);
    procedure SetFolderDialog(const Value: TCHFolderDialog);
  protected

  public
    constructor Create(AOwner: TCHEditBtn); virtual;
  published
    property Style : TEditModus read FEditModus Write SetEditModus;
    property DateDialog : TCHDateDialog read FDateDialog Write SetDateDialog;
    property FileDialog : TOpenDialog read FFileDialog Write SetFileDialog;
    property FolderDialog : TCHFolderDialog read FFolderDialog Write SetFolderDialog;
  end;

  TCHEditBtn = class(TCHEdit)
  private
    FForm : TCustomForm;
    FButton : TSpeedButton;
    FOnButtonClick: TNotifyEvent;
    FEditStyle : TEditStyle;

    FOnDialogClose: TNotifyEvent;
    FOnDialogCancel: TNotifyEvent;
    procedure SetButtonSytle;
    procedure SetButtonPos;
  protected
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure GetLeftTop(WC : TWinControl; var LT : TPoint);
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  public
    FOnDialogOK: TNotifyEvent;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetEditRect;
    function CheckDate : Boolean;

  published
    property OnButtonClick : TNotifyEvent read FOnButtonClick Write FOnButtonClick;
    property OnDialogClose : TNotifyEvent read FOnDialogClose Write FOnDialogClose;
    property OnDialogCancel : TNotifyEvent read FOnDialogCancel Write FOnDialogCancel;
    property OnDialogOK : TNotifyEvent read FOnDialogOK Write FOnDialogOK;

    property EditStyle : TEditStyle read FEditStyle write FEditStyle;
  end;

procedure Register;

implementation

uses
  Types;

{$R CHEditBtn.res}

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHEditBtn]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := GetParentForm(TControl(AOwner));
  FEditStyle := TEditStyle.Create(Self);

  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];

  FButton := TSpeedButton.Create(Self);
  FButton.Parent := Self;
  FButton.Cursor:=crArrow;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHEditBtn.Destroy;
begin
  FButton := nil;

  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.SetButtonSytle;
begin
  if FEditStyle.FEditModus = emUser then
  begin
    FButton.Glyph := nil;
    FButton.OnClick := FEditStyle.UserClick;
  end
  else if FEditStyle.FEditModus = emDate then
  begin
    FButton.Glyph.Handle := LoadBitmap(hInstance, 'DATE');
    FButton.OnClick := FEditStyle.DateClick;
  end
  else if FEditStyle.FEditModus = emFilename then
  begin
    FButton.Glyph.Handle := LoadBitmap(hInstance, 'FILENAME');
    FButton.OnClick := FEditStyle.FileClick;
  end
  else if FEditStyle.FEditModus = emDirectory then
  begin
    FButton.Glyph.Handle := LoadBitmap(hInstance, 'DIRECTORY');
    FButton.OnClick := FEditStyle.DirectoryClick;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TEditStyle.DirectoryClick(Sender: TObject);
var
  bOk : Boolean;
begin
  if Assigned(FOwner.FOnButtonClick) then
    FOwner.FOnButtonClick(self);

  bOk := False;
  if FolderDialog <> nil then
    if TCHFolderDialog(FolderDialog).Execute then
    begin
      FOwner.Text := TCHFolderDialog(FolderDialog).Path;
      bOk := True;
    end;

  FOwner.SetFocus;

  if Assigned(FOwner.FOnDialogClose) then
    FOwner.FOnDialogClose(self);

  if bOk then
  begin
    if Assigned(FOwner.FOnDialogOK) then
      FOwner.FOnDialogOK(Self);
  end
  else
  begin
    if Assigned(FOwner.FOnDialogCancel) then
      FOwner.FOnDialogCancel(self);
  end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TEditStyle.FileClick(Sender: TObject);
var
  bOk : Boolean;
begin
  if Assigned(FOwner.FOnButtonClick ) then
    FOwner.FOnButtonClick(self);

  bOk := False;
  if FileDialog <> nil then
    if TOpenDialog(FileDialog).Execute then
    begin
      FOwner.Text := TOpenDialog(FileDialog).FileName;
      bOk := True;
    end;

  FOwner.SetFocus;

  if Assigned(FOwner.FOnDialogClose) then
    FOwner.FOnDialogClose(self);

  if bOk then
  begin
    if Assigned(FOwner.FOnDialogOK) then
      FOwner.FOnDialogOK(Self);
  end
  else
  begin
    if Assigned(FOwner.FOnDialogCancel) then
      FOwner.FOnDialogCancel(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TEditStyle.UserClick(Sender: TObject);
begin
  if Assigned(FOwner.FOnButtonClick ) then
    FOwner.FOnButtonClick(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TEditStyle.DateClick(Sender: TObject);
var
  nLeft, nTop : Integer;
  nLT : TPoint;
  dCheck : TDate;
begin
  if Assigned(FOwner.FOnButtonClick ) then
    FOwner.FOnButtonClick(self);

  if DateDialog <> nil then
  begin
    dCheck := Now;
    if Trim(FOwner.Text) <> '' then
    begin
      try
        dCheck := StrToDate(FOwner.Text);
      except
        FOwner.Text := '';
      end;
    end;

    TCHDateDialog(DateDialog).Execute(TEdit(FOwner));
    TCHDateDialog(DateDialog).FCalendar.Date := dCheck;

    if TCHDateDialog(DateDialog).Position = psWinControl then
    begin
      nLT.X := 0;
      nLT.Y := 0;
      FOwner.GetLeftTop(FOwner, nLT);

      nTop := nLT.Y;
      nLeft := nLT.X;

      // Top
      if (nTop + TCHDateDialog(DateDialog).Height) < FOwner.FForm.ClientHeight then
        TCHDateDialog(DateDialog).Top := FOwner.Height + nTop
      else
        TCHDateDialog(DateDialog).Top := nTop - TCHDateDialog(DateDialog).Height;

      // Left
      if (nLeft + TCHDateDialog(DateDialog).Width) < FOwner.FForm.ClientWidth then
        TCHDateDialog(DateDialog).Left := nLeft
      else
        TCHDateDialog(DateDialog).Left := nLeft - (TCHDateDialog(DateDialog).Width - FOwner.Width);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.GetLeftTop(WC : TWinControl; var LT : TPoint);
begin
  if WC = FForm then
  begin
    Exit;
  end
  else
  begin
    LT.X := LT.X + WC.Left;
    LT.Y := LT.Y + WC.Top;
    GetLeftTop(WC.Parent, LT);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.SetButtonPos;
begin
  FButton.Height := ClientHeight -2;
  FButton.Width := ClientHeight -2;
  FButton.Left := ClientWidth - FButton.Width -1;
  FButton.Top := 1;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.WMSize(var Message: TWMSize);
begin
  inherited;
  if FButton <> nil then
  begin
    SetButtonPos;
    SetEditRect;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.CreateWnd;
begin
  inherited CreateWnd;
  if Assigned(FButton) then
    SetEditRect;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHEditBtn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
   if Operation <> opRemove then
      Exit;
   if AComponent = FEditStyle.FFileDialog then
      FEditStyle.FFileDialog := nil;
   if AComponent = FEditStyle.FFolderDialog then
     FEditStyle.FFolderDialog := nil;
   if AComponent = FEditStyle.FDateDialog then
     FEditStyle.FDateDialog := nil;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TEditStyle.Create(AOwner: TCHEditBtn);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TEditStyle.SetEditModus(const Value: TEditModus);
begin
  if FEditModus <> Value then
  begin
    FEditModus := Value;
    FOwner.SetButtonSytle;
  end;
end;



function TCHEditBtn.CheckDate: Boolean;
var
  dCheck : TDate;
begin
  Result := True;
  if FEditStyle.FEditModus = emDate then
  begin
    if Trim(Text) <> '' then
    begin
      dCheck := StrToDateDef(Text, 0);
      if dCheck = 0 then
      begin
        ShowMessage('Ungültiges Datum: '+Text);
        Result := False;
        Clear;
      end
    end;
  end;
end;

procedure TEditStyle.SetDateDialog(const Value: TCHDateDialog);
begin
  if Assigned(FDateDialog) then
    FDateDialog.RemoveFreeNotification(FDateDialog);

  FDateDialog := Value;

  if Assigned(FDateDialog) then
    FDateDialog.FreeNotification(FDateDialog);
end;

procedure TEditStyle.SetFileDialog(const Value: TOpenDialog);
begin
  if Assigned(FFileDialog) then
    FFileDialog.RemoveFreeNotification(FFileDialog);

  FFileDialog := Value;

  if Assigned(FFileDialog) then
    FFileDialog.FreeNotification(FFileDialog);
end;

procedure TEditStyle.SetFolderDialog(const Value: TCHFolderDialog);
begin
  if Assigned(FFolderDialog) then
    FFolderDialog.RemoveFreeNotification(FFolderDialog);

  FFolderDialog := Value;

  if Assigned(FFolderDialog) then
    FFolderDialog.FreeNotification(FFolderDialog);
end;

end.
