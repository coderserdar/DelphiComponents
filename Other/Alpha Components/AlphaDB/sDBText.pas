unit sDBText;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, DBCtrls, db,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  sCommonData, sLabel;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBText = class(TsLabel)
  private
    FDataLink: TFieldDataLink;
    FBoundLabel: TsBoundLabel;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldText: string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function GetLabelText: string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    procedure WndProc(var Message: TMessage); override;
    property Field: TField read GetField;
  published
    property AutoSize default False;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
  end;


implementation

uses dbconsts, acntUtils;


procedure TsDBText.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;


constructor TsDBText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  ShowAccelChar := False;
  AutoSize := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FBoundLabel := TsBoundLabel.Create(Self, nil);
end;


procedure TsDBText.DataChange(Sender: TObject);
begin
  Caption := GetFieldText
end;


destructor TsDBText.Destroy;
begin
  FreeAndNil(FBoundLabel);
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;


function TsDBText.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and FDataLink.ExecuteAction(Action);
end;


function TsDBText.GetDataField: string;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.FieldName
  else
    Result := '';
end;


function TsDBText.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;


function TsDBText.GetField: TField;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.Field
  else
    Result := nil;
end;


function TsDBText.GetFieldText: string;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.Field) then
    Result := FDataLink.Field.DisplayText
  else
    Result := iff(csDesigning in ComponentState, Name, '');
end;


function TsDBText.GetLabelText: string;
begin
  if csPaintCopy in ControlState then
    Result := GetFieldText
  else
    Result := Caption;
end;


procedure TsDBText.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);

  if Assigned(BoundLabel) and BoundLabel.Active and (BoundLabel.FTheLabel = nil) then
    BoundLabel.UpdateVisibility;
end;


procedure TsDBText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;


procedure TsDBText.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then begin
    if Value and FDataLink.DataSourceFixed then
      DatabaseError('Operation not allowed in a DBCtrlGrid');

    inherited SetAutoSize(Value);
  end;
end;


procedure TsDBText.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;


procedure TsDBText.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;

  if Value <> nil then
    Value.FreeNotification(Self);
end;


function TsDBText.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and FDataLink.UpdateAction(Action);
end;


function TsDBText.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;


procedure TsDBText.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  inherited;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;

end.
