unit PDBCheck;

// TPDBCheck version 1.50
// Freeware Component for for D3,D4,D5,D6
// Copyright © 2000-2001 by Peric
// Birthday of Component 28.08.2001
// E-mail: pericddn@ptt.yu
// http://www.ptt.yu/korisnici/p/e/pericddn/
// If I' find any errors or rubbish in TPDBCheck please send me Your suggest or Reclamation.
// Very special thanks to:
// Paul Bailey


{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE PDJ_D3}
{$ELSE}
  {$IFDEF VER120}
    {$DEFINE PDJ_D4}
  {$ELSE}
    {$DEFINE PDJ_D5Up}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE PDJ_D6}
{$ENDIF}


interface

uses
      Messages,
{$IFDEF FPC}
      LCLIntf, lMessages, lresources,
{$ELSE}
       Windows,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PCheck,DBCtrls, DB;

{$IFDEF FPC}
  {$MODE Delphi}
{$ELSE}
  {$R PDBCheck.res}
{$ENDIF}

type
  TPCustomDBCheck = class(TPCustomCheck)

  private

    FVersion: string;
    FReadOnly:boolean;
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetDataField (const Value: string);
    procedure SetDataSource (Value: TDataSource);
    procedure DataChange (Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure SetReadOnly(value:boolean);
    procedure CmDialogChar(var Message: TCMDialogChar);
              message CM_DIALOGCHAR;
  protected

    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Click;override;
    property Version: string read GetVersion write SetVersion;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Caption;
    property ReadOnly:boolean read FReadOnly write SetReadOnly default False;
    
  public

    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

  end;

  TPDBCheck = class(TPCustomDBCheck)

  published

    property Transparent;
    property ShowFocused;
    property Flat;
    property Version;
    property DataField;
    property DataSource;
    property Caption;
    property ReadOnly;
    property HotTrack;
    property OnMouseEnter;
    property OnMouseExit;
    property Alignment;
    property ImageType;
    property ShowHandCursor;
    property PlayMusic;
    property Style;
    property ColorSipleMargine;
    property HintSecondLine;
    property Enabled;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentFont;
    property Color;
    property ParentColor;
    property Visible;
    property ShowHint;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnEnter;
    property OnExit;
    property TabStop default True;
    property TabOrder;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    
    {$IFDEF PDJ_D5up}
    property OnContextPopup;
    {$ENDIF}
      {$IFDEF PDJ_D3}
  {$ELSE}
    property DragKind;
    property Anchors;
    property Constraints;
    property BiDiMode;
    property ParentBiDiMode;
    property OnStartDock;
    property OnEndDock;
  {$ENDIF}
   end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PDJ', [TPDBCheck]);
end;

constructor TPCustomDBCheck.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FReadOnly:=False;
  FVersion:='Version 1.50, Copyright © 2000-2001 by Peric, E-mail: pericddn@ptt.yu';
end;

destructor TPCustomDBCheck.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  inherited Destroy;
end;

procedure TPCustomDBCheck.CmDialogChar(var Message: TCMDialogChar);
begin

  if FReadOnly then Exit;
  if FDatalink.Field = nil then Exit;
  {$IFDEF FPC}
  {$ELSE}
  inherited;
  {$ENDIF}
  UpdateData(Self);
end;

procedure TPCustomDBCheck.SetVersion(const Value: string);
begin
  FVersion:=FVersion;
end;

function TPCustomDBCheck.GetVersion: string;
begin
  Result:=FVersion;
end;

procedure TPCustomDBCheck.SetReadOnly(value:boolean);
begin
if value<>FReadOnly then
FReadOnly:=value;
end;

procedure TPCustomDBCheck.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Begin
if not FDatalink.Active then Exit;
if FReadOnly then Exit;
if FDatalink.Field = nil then Exit;
//Fixed  Antonio Torregrosa {26.11.2001}
if not(FDataLink.DataSource.state in [dsEdit,dsInsert]) then exit;
inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPCustomDBCheck.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TPCustomDBCheck.DataChange (Sender: TObject);
begin
if FDatalink.Field = nil then begin
Checked:=False;
Exit;
end
else  
begin
    IF FDataLink.Field.AsBoolean<>Checked then Checked := FDataLink.Field.AsBoolean;
    Repaint;
  end;
end;

function TPCustomDBCheck.GetDataField: string;
begin
  Result := FDataLink.Fieldname;
end;

function TPCustomDBCheck.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TPCustomDBCheck.SetDataField (const Value: string);
begin
  FDataLink.Fieldname := Value;
end;

procedure TPCustomDBCheck.SetDataSource (Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TPCustomDBCheck.UpdateData(Sender: TObject);
VAr Chk : boolean;
begin
if FReadOnly then Exit;
  Chk := Checked;
  if FDataLink.Edit then
  begin
    if FDataLink.Field.DataType = ftBoolean then FDataLink.Field.AsBoolean := Chk;
  end;
end;


procedure TPCustomDBCheck.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
  UpdateData(Self);
end;

procedure TPCustomDBCheck.Click;
begin
  inherited;
  UpdateData(Self);
end;
{$IFDEF FPC}
initialization
  {$i PDBCheck.lrs}
{$ENDIF}
end.
