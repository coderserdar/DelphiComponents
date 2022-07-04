{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29
}
unit SMDBStat;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, DB, DBCtrls;

type
{ TSMDBStatusLbl }

  TSMDBTypeEvents = (dbeActiveChanged, dbeEditingChanged,
                     dbeDataSetChanged, dbeDataSetScrolled,
                     dbeLayoutChanged, dbeRecordChanged,
                     dbeUpdateData);
  TSMDBEvent = procedure(Sender: TObject; SMDBTypeEvent: TSMDBTypeEvents; Field: TField; var strCaption: TCaption) of object;

  TSMDBStatusLbl = class(TCustomLabel)
  private
    { Private declarations }
    FDataLink: TDataLink;
    FSMDBEvent: TSMDBEvent;

    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetDatasetState: TDataSetState;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateStatus(SMDBTypeEvent: TSMDBTypeEvents; Field: TField); virtual;
    property Caption;
    property DatasetState: TDataSetState read GetDatasetState;
  published
    { Published declarations }
    property SMDBEvent: TSMDBEvent read FSMDBEvent write FSMDBEvent;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Layout;
    property Align;
    property Alignment;
    property Color;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TSMDBButton }

  TSMDBButton = class(TButton)
  private
    FWordWrap: Boolean;

    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldText: string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetWordWrap(Value: Boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property Align;
    property Color;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation
uses DBTables;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBStatusLbl, TSMDBButton]);
end;

{ TStatusDataLink }

type
  TStatusDataLink = class(TDataLink)
  private
    FLabel: TSMDBStatusLbl;
  protected
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(ALabel: TSMDBStatusLbl);
    destructor Destroy; override;
  end;

constructor TStatusDataLink.Create(ALabel: TSMDBStatusLbl);
begin
  inherited Create;

  FLabel := ALabel;
end;

destructor TStatusDataLink.Destroy;
begin
  FLabel := nil;
  inherited Destroy;
end;

procedure TStatusDataLink.DataSetScrolled(Distance: Integer);
begin
  if FLabel <> nil then
    FLabel.UpdateStatus(dbeDataSetScrolled, nil);
end;

procedure TStatusDataLink.EditingChanged;
begin
  if FLabel <> nil then
    FLabel.UpdateStatus(dbeEditingChanged, nil);
end;

procedure TStatusDataLink.DataSetChanged;
begin
  if FLabel <> nil then
    FLabel.UpdateStatus(dbeDataSetChanged, nil);
end;

procedure TStatusDataLink.LayoutChanged;
begin
  if FLabel <> nil then
    FLabel.UpdateStatus(dbeLayoutChanged, nil);
end;

procedure TStatusDataLink.ActiveChanged;
begin
  if FLabel <> nil then
    FLabel.UpdateStatus(dbeActiveChanged, nil);
end;

procedure TStatusDataLink.RecordChanged(Field: TField);
begin
  if FLabel <> nil then
    FLabel.UpdateStatus(dbeRecordChanged, Field);
end;

procedure TStatusDataLink.UpdateData;
begin
  if FLabel <> nil then
    FLabel.UpdateStatus(dbeUpdateData, nil);
end;

{ TSMDBStatusLbl }

constructor TSMDBStatusLbl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [{csSetCaption, }csReplicatable];
  ShowAccelChar := False;
  AutoSize := False;
  FDataLink := TStatusDataLink.Create(Self);
end;

destructor TSMDBStatusLbl.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

function TSMDBStatusLbl.GetDatasetState: TDataSetState;
begin
  if DataSource <> nil then
    Result := DataSource.State
  else
    Result := dsInactive;
end;

procedure TSMDBStatusLbl.Paint;
begin
  inherited Paint;

end;


procedure TSMDBStatusLbl.UpdateStatus(SMDBTypeEvent: TSMDBTypeEvents; Field: TField);
var strCaption: TCaption;
begin
  if not (csDesigning in ComponentState) then
    if Assigned(FSMDBEvent) then
    begin
      strCaption := Caption;
      FSMDBEvent(Self, SMDBTypeEvent, Field, strCaption);
      Caption := strCaption
    end;
end;

procedure TSMDBStatusLbl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (FDataLink <> nil) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

function TSMDBStatusLbl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TSMDBStatusLbl.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
{$IFDEF WIN32}
  if Value <> nil then
    Value.FreeNotification(Self);
{$ENDIF}
  if not (csLoading in ComponentState) then
    UpdateStatus(dbeActiveChanged, nil);
end;


{ TSMDBButton }

constructor TSMDBButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWordWrap := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TSMDBButton.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

procedure TSMDBButton.SetWordWrap(Value: Boolean);
begin
  if (FWordWrap <> Value) then
  begin
    FWordWrap := Value;
    ReCreateWnd;
  end;
end;

procedure TSMDBButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if FWordWrap then
    Params.Style := Params.Style or BS_MULTILINE
  else
    Params.Style := Params.Style and not BS_MULTILINE;
end;

procedure TSMDBButton.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TSMDBButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and
     (FDataLink <> nil) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

function TSMDBButton.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TSMDBButton.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TSMDBButton.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TSMDBButton.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TSMDBButton.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TSMDBButton.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then
      Result := Name
    else
      Result := '';
end;

procedure TSMDBButton.DataChange(Sender: TObject);
begin
  Caption := GetFieldText;
end;

procedure TSMDBButton.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

end.
