//---------------------------------------------------------------------------
//  TVolgaFindEdit - inherited from TCustomEdit with linked dataset for
//  searching  thought it. It supports navigate search and filter search
//---------------------------------------------------------------------------
//  Copyright © 2000-2002, Olga Vlasova, Russia
//  http://www.volgadb.com
//  E-mail: info@volgadb.com
//---------------------------------------------------------------------------

unit VolFndEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, extctrls, DB, dbctrls;

type
  TVolgaFindStyle = (vfsNavigate, vfsFilter, vfsCustomFilter);
  TVolgaFindMode = (vfmFirstPos, vfmAnyPos);

  TVolgaFindEdit = class;

  TVolgaCustomFilterEvent = procedure(Sender: TVolgaFindEdit;
  Dataset: TDataset; var Accept: Boolean) of object;

  TVolgaFindEdit = class(TCustomEdit)
  private
    { Private declarations }
    FTimer: TTimer;
    FOldFiltered: Boolean;
    FOldFilterRecord: TFilterRecordEvent;
    FDataLink: TFieldDataLink;
    FIgnoreCase: Boolean;
    FFindMode: TVolgaFindMode;
    FFindStyle: TVolgaFindStyle;
    FSearchText: string;
    FCustomFilter: TVolgaCustomFilterEvent;
    procedure ActiveChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetFindMode(const Value: TVolgaFindMode);
    procedure SetFindStyle(const Value: TVolgaFindStyle);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure FTimerTimer(Sender: TObject);
    procedure AFilterRecord(DataSet: TDataSet;
      var Accept: Boolean);
  protected
    { Protected declarations }
    procedure Change; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Find(AText: string);
    procedure ClearSearch;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property FindStyle: TVolgaFindStyle read FFindStyle write SetFindStyle default
      vfsNavigate;
    property FindMode: TVolgaFindMode read FFindMode write SetFindMode default vfmFirstPos;
    property IgnoreCase: Boolean read FIgnoreCase write SetIgnoreCase default true;
    property OnCustomFilter: TVolgaCustomFilterEvent read FCustomFilter write FCustomFilter;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Volga', [TVolgaFindEdit]);
end;

{ TVolgaFindEdit }

constructor TVolgaFindEdit.Create(AOwner: TComponent);
begin
  inherited;
  FFindStyle := vfsNavigate;
  FFindMode := vfmFirstPos;
  FIgnoreCase := true;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.Interval := 100;
  FTimer.OnTimer := FTimerTimer;
  FSearchText := '';
  FOldFiltered := false;
  FOldFilterRecord := nil;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TVolgaFindEdit.Destroy;
begin
  if not (csDestroying in Owner.ComponentState) then
    try
      if FDataLink.Active and (FFindStyle = vfsFilter) then
      begin
        FDataLink.DataSet.OnFilterRecord := FOldFilterRecord;
        FDataLink.DataSet.Filtered := FOldFiltered;
      end;
    except ; end;
  FDataLink.Control := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TVolgaFindEdit.Change;
begin
  FTimer.Enabled := false;               //остановить предыд.таймер
  FTimer.Enabled := true;
  FSearchText := Text;
  inherited;
end;

procedure TVolgaFindEdit.ClearSearch;
begin
  FTimer.Enabled := false;
  FSearchText := '';
  if FFindStyle <> vfsNavigate then begin
    FDataLink.DataSet.OnFilterRecord := FOldFilterRecord;
    FDataLink.DataSet.Filtered := FOldFiltered;
  end;
end;

procedure TVolgaFindEdit.FTimerTimer(Sender: TObject);
begin
  FTimer.Enabled := false;
  ActiveChange(Self);
  if FSearchText='' then
    if FFindStyle = vfsFilter then begin
      FDataLink.DataSet.OnFilterRecord := FOldFilterRecord;
      FDataLink.DataSet.Filtered := FOldFiltered;
    end else
  else    //выполнить поиск после задержки
  begin
    if not FDataLink.Active or (FDataLink.Field = nil) then Exit;
    case FFindStyle of
      vfsNavigate:
        if IgnoreCase then
          FDataLink.DataSet.Locate(DataField, FSearchText, [loCaseInsensitive, loPartialKey])
        else
          FDataLink.DataSet.Locate(DataField, FSearchText, [loPartialKey]);
      vfsFilter, vfsCustomFilter:
        FDataLink.DataSet.Filtered := true;
    end;
  end;
end;

procedure TVolgaFindEdit.Find(AText: string);
begin
  FSearchText := AText;
  FTimerTimer(FTimer);
end;

procedure TVolgaFindEdit.AFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept := true;
  if FOldFiltered and Assigned(FOldFilterRecord) then
    FOldFilterRecord(DataSet, Accept);
  if not Accept then Exit;
  if (FFindStyle = vfsCustomFilter) and Assigned(FCustomFilter) then
    FCustomFilter(Self, FDataLink.DataSet, Accept)
  else if FFindMode = vfmFirstPos then
    if IgnoreCase then
      Accept := Pos(AnsiUpperCase(FSearchText),
        AnsiUpperCase(DataSet.FieldByName(DataField).AsString)) = 1
    else
      Accept := Pos(FSearchText, DataSet.FieldByName(DataField).AsString) = 1
  else if IgnoreCase then
    Accept := Pos(AnsiUpperCase(FSearchText),
      AnsiUpperCase(DataSet.FieldByName(DataField).AsString)) > 0
  else
    Accept := Pos(FSearchText, DataSet.FieldByName(DataField).AsString) > 0
end;

procedure TVolgaFindEdit.ActiveChange(Sender: TObject);
var Func1,Func2:TFilterRecordEvent;
begin
  if (FFindStyle = vfsNavigate) or (FDataLink.DataSet = nil) then Exit;
  Func1 := FDataLink.DataSet.OnFilterRecord;
  Func2 := AFilterRecord;
  if FDataLink.Active and (@Func1 <> @Func2) and (FSearchText>'') then
  begin
    FOldFilterRecord := FDataLink.DataSet.OnFilterRecord;
    FOldFiltered := FDataLink.DataSet.Filtered;
    FDataLink.DataSet.OnFilterRecord := AFilterRecord;
{  end
  else
  begin
    FOldFiltered := false;
    FOldFilterRecord := nil;}
  end;
end;

function TVolgaFindEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TVolgaFindEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TVolgaFindEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TVolgaFindEdit.SetDataField(const Value: string);
begin
  if Value > '' then
    FDataLink.FieldName := Value;
end;

procedure TVolgaFindEdit.SetFindMode(const Value: TVolgaFindMode);
begin
  if FFindStyle = vfsNavigate then
    FFindMode := vfmFirstPos
  else
    FFindMode := Value;
end;

procedure TVolgaFindEdit.SetFindStyle(const Value: TVolgaFindStyle);
begin
  FFindStyle := Value;
  if FFindStyle = vfsNavigate then FFindMode := vfmFirstPos;
  ActiveChange(Self);
end;

procedure TVolgaFindEdit.SetIgnoreCase(const Value: Boolean);
begin
  FIgnoreCase := Value;
end;

procedure TVolgaFindEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
    begin
      DataSource := nil;
    end;
  end;
end;

end.

