unit acDBBtns;
{$I sDefs.inc}

interface

uses Windows, Controls, Classes, DB, DBCtrls, Dialogs, Graphics, ImgList,
  sButton, sDialogs, sSpeedButton, sBitBtn, sCommonData;

resourcestring
  rsDeleteRecordQuestion = 'Delete this record?';


type
  IsDBNavControl = interface
    procedure EditingChanged;
    procedure DataSetChanged;
    procedure ActiveChanged;
  end;


  TsDBNavBtnClick = procedure(var AllowAction: Boolean; Sender: TObject) of object;
  TsDBNavBtnClicked = procedure(Sender: TObject) of object;

  TsNavButtonDataLink = class;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBButton = class(TsButton, IsDBNavControl)
  private
    FDataLink: TsNavButtonDataLink;
    FBtnFunction: TNavigateBtn;
    FBeforeAction: TsDBNavBtnClick;
    FAfterAction: TsDBNavBtnClicked;
    FConfirmDelete: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetBtnFunction(const Value: TNavigateBtn);
  protected
    procedure DataSetChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
  public
    function GetImages: TCustomImageList; override;
    function CurrentImageIndex: integer; override;
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ButtonFunction: TNavigateBtn read FBtnFunction write SetBtnFunction;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property BeforeAction: TsDBNavBtnClick read FBeforeAction write FBeforeAction;
    property AfterAction: TsDBNavBtnClicked read FAfterAction write FAfterAction;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBSpeedButton = class(TsSpeedButton, IsDBNavControl)
  private
    FDataLink: TsNavButtonDataLink;
    FBtnFunction: TNavigateBtn;
    FBeforeAction: TsDBNavBtnClick;
    FAfterAction: TsDBNavBtnClicked;
    FConfirmDelete: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetBtnFunction(const Value: TNavigateBtn);
  protected
    procedure DataSetChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
  public
    function GetImages: TCustomImageList; override;
    function CurrentImageIndex: integer; override;
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ButtonFunction: TNavigateBtn read FBtnFunction write SetBtnFunction;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property BeforeAction: TsDBNavBtnClick read FBeforeAction write FBeforeAction;
    property AfterAction: TsDBNavBtnClicked read FAfterAction write FAfterAction;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBBitBtn = class(TsBitBtn, IsDBNavControl)
  private
    FDataLink: TsNavButtonDataLink;
    FBtnFunction: TNavigateBtn;
    FBeforeAction: TsDBNavBtnClick;
    FAfterAction: TsDBNavBtnClicked;
    FConfirmDelete: Boolean;
    FColored: boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetBtnFunction(const Value: TNavigateBtn);
    procedure SetColored(const Value: boolean);
  protected
    function GetColorTone: TColor; override;
    procedure DataSetChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
  public
    function GetImages: TCustomImageList; override;
    function CurrentImageIndex: integer; override;
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ButtonFunction: TNavigateBtn read FBtnFunction write SetBtnFunction;
    property Colored: boolean read FColored write SetColored default False;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property BeforeAction: TsDBNavBtnClick read FBeforeAction write FBeforeAction;
    property AfterAction: TsDBNavBtnClicked read FAfterAction write FAfterAction;
  end;


  TsNavButtonDataLink = class(TDataLink)
  private
    FNavigator: IsDBNavControl;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: IsDBNavControl);
    destructor Destroy; override;
  end;

function DBGlyphIndex(Func: TNavigateBtn): integer;
function DBGlyphColor(SkinData: TsCommonData; Func: TNavigateBtn; State: integer): TColor;

implementation


uses sGraphUtils, sGlyphUtils, sThirdParty, acntTypes, sSkinManager;


const
  aGlyphSkinColors: array [boolean, TNavigateBtn] of TacPaletteColors = (
    (pcBtnBlueText, pcBtnBlueText, pcBtnBlueText, pcBtnBlueText, pcBtnRedText, pcBtnRedText, pcBtnBlueText, pcBtnGreenText, pcBtnRedText, pcBtnBlueText{$IFDEF DELPHI_XE2}, pcBtnGreenText, pcBtnRedText{$ENDIF}),
    (pcBtnBlueTextActive, pcBtnBlueTextActive, pcBtnBlueTextActive, pcBtnBlueTextActive, pcBtnRedTextActive, pcBtnRedTextActive, pcBtnBlueTextActive, pcBtnGreenTextActive, pcBtnRedTextActive, pcBtnBlueTextActive{$IFDEF DELPHI_XE2}, pcBtnGreenTextActive, pcBtnRedTextActive{$ENDIF}));


function DBGlyphColor(SkinData: TsCommonData; Func: TNavigateBtn; State: integer): TColor;
begin
  if SkinData.Skinned then
    if SkinData.SkinManager.Effects.DiscoloredGlyphs and (State = 0) then
      Result := GetFontColor(SkinData.FOwnerControl, SkinData.SkinIndex, SkinData.SkinManager, State, SkinData)
    else
      Result := SkinData.SkinManager.Palette[aGlyphSkinColors[State > 0, Func]]
  else
    case Func of
      nbRefresh,
      nbEdit,
      nbFirst,
      nbPrior,
      nbNext,
      nbLast:    Result := 14389760;

      nbCancel,
      nbInsert,
      nbDelete:  Result := 180;

      nbPost:    Result := 2138929;

      else       Result := 0;
    end;
end;


function DBGlyphIndex(Func: TNavigateBtn): integer;
begin
  case Func of
    nbFirst:   Result := 14;
    nbPrior:   Result := 15;
    nbNext:    Result := 16;
    nbLast:    Result := 17;
    nbInsert:  Result := 11;
    nbDelete:  Result := 18;
    nbEdit:    Result := 12;
    nbPost:    Result := 9;
    nbCancel:  Result := 10;
    nbRefresh: Result := 13;
    else       Result := -1;
  end;
end;


procedure DataSetFunction(aDataSet: TDataSet; Func: TNavigateBtn; ConfirmDelete: boolean);
begin
  case Func of
    nbInsert:  aDataSet.Insert;
    nbEdit:    aDataSet.Edit;
    nbPost:    aDataSet.Post;
    nbCancel:  aDataSet.Cancel;
    nbRefresh: aDataSet.Refresh;
    nbFirst:   aDataSet.First;
    nbPrior:   aDataSet.Prior;
    nbNext:    aDataSet.Next;
    nbLast:    aDataSet.Last;
    nbDelete:  if not ConfirmDelete or (sMessageDlg(rsDeleteRecordQuestion, mtConfirmation, mbOKCancel, 0) <> idCancel) then
      aDataSet.Delete;
  end;
end;


procedure DoEditingChanged(aBtn: TControl; aDataLink: TDataLink; aFunc: TNavigateBtn);
var
  CanModify: boolean;
begin
  CanModify := aDataLink.Active and aDataLink.DataSet.CanModify;
  case aFunc of
    nbInsert:  aBtn.Enabled := CanModify;
    nbEdit:    aBtn.Enabled := CanModify and not aDataLink.Editing;
    nbPost:    aBtn.Enabled := CanModify and aDataLink.Editing;
    nbCancel:  aBtn.Enabled := CanModify and aDataLink.Editing;
    nbRefresh: aBtn.Enabled := CanModify;
  end;
end;


procedure DoDataSetChanged(aBtn: TControl; aDataLink: TDataLink; aFunc: TNavigateBtn);
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := aDataLink.Active and not aDataLink.DataSet.Bof;
  DnEnable := aDataLink.Active and not aDataLink.DataSet.Eof;
  case aFunc of
    nbFirst:  aBtn.Enabled := UpEnable;
    nbPrior:  aBtn.Enabled := UpEnable;
    nbNext:   aBtn.Enabled := DnEnable;
    nbLast:   aBtn.Enabled := DnEnable;
    nbDelete: aBtn.Enabled := aDataLink.Active and aDataLink.DataSet.CanModify and not (aDataLink.DataSet.Bof and aDataLink.DataSet.Eof);
  end;
end;


procedure TsNavButtonDataLink.ActiveChanged;
begin
  if FNavigator <> nil then
    FNavigator.ActiveChanged;
end;


constructor TsNavButtonDataLink.Create(ANav: IsDBNavControl);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;


procedure TsNavButtonDataLink.DataSetChanged;
begin
  if FNavigator <> nil then
    FNavigator.DataSetChanged;
end;


destructor TsNavButtonDataLink.Destroy;
begin
  FNavigator := nil;
  inherited;
end;


procedure TsNavButtonDataLink.EditingChanged;
begin
  if FNavigator <> nil then
    FNavigator.EditingChanged;
end;


procedure TsDBButton.ActiveChanged;
begin
  if not FDataLink.Active then begin
    Enabled := False
  end
  else begin
    DataSetChanged;
    EditingChanged;
  end;
end;


procedure TsDBButton.Click;
var
  AllowAction: Boolean;
begin
  inherited;
  AllowAction := True;
  if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
    FBeforeAction(AllowAction, Self);

  if (DataSource <> nil) and (DataSource.State <> dsInactive) and AllowAction then
    DataSetFunction(DataSource.DataSet, FBtnFunction, FConfirmDelete);

  if Assigned(FAfterAction) then
    FAfterAction(Self);
end;


constructor TsDBButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TsNavButtonDataLink.Create(Self);
  FConfirmDelete := True;
  SetBtnFunction(nbFirst);
  Enabled := False;
end;


function TsDBButton.CurrentImageIndex: integer;
begin
  if Images = nil then
    Result := DBGlyphIndex(FBtnFunction)
  else
    Result := inherited CurrentImageIndex;
end;


procedure TsDBButton.DataSetChanged;
begin
  if not (csLoading in ComponentState) then
    DoDataSetChanged(Self, FDataLink, FBtnFunction);
end;


destructor TsDBButton.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;


procedure TsDBButton.EditingChanged;
begin
  if not (csLoading in ComponentState) then
    DoEditingChanged(Self, FDataLink, FBtnFunction);
end;


function TsDBButton.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;


function TsDBButton.GetImages: TCustomImageList;
var
  res: TCustomImageList;
begin
  res := inherited GetImages;
  if res = nil then // If glyph is not defined
    Result := acCharImages
  else
    Result := res;
end;


procedure TsDBButton.SetBtnFunction(const Value: TNavigateBtn);
begin
  if Value <> FBtnFunction then
    FBtnFunction := Value;
end;


procedure TsDBButton.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDataLink.DataSource then
    FDataLink.DataSource := Value;

  if not (csLoading in ComponentState) then
    ActiveChanged;

  if Value <> nil then
    Value.FreeNotification(Self);
end;


procedure TsDBSpeedButton.ActiveChanged;
begin
  if not FDataLink.Active then
    Enabled := False
  else begin
    DataSetChanged;
    EditingChanged;
  end;
end;


procedure TsDBSpeedButton.Click;
var
  AllowAction: Boolean;
begin
  inherited;
  AllowAction := True;
  if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
    FBeforeAction(AllowAction, Self);

  if (DataSource <> nil) and (DataSource.State <> dsInactive) and AllowAction then
    DataSetFunction(DataSource.DataSet, FBtnFunction, FConfirmDelete);

  if Assigned(FAfterAction) then
    FAfterAction(Self);
end;


constructor TsDBSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TsNavButtonDataLink.Create(Self);
  FConfirmDelete := True;
  SetBtnFunction(nbFirst);
  Enabled := False;
end;


function TsDBSpeedButton.CurrentImageIndex: integer;
begin
  if (Images = nil) and Glyph.Empty then begin
//    GlyphColorTone := DBGlyphColor(SkinData, FBtnFunction, CurrentState);
    Result := DBGlyphIndex(FBtnFunction)
  end
  else
    Result := inherited CurrentImageIndex;
end;


procedure TsDBSpeedButton.DataSetChanged;
begin
  if not (csLoading in ComponentState) then
    DoDataSetChanged(Self, FDataLink, FBtnFunction);
end;


destructor TsDBSpeedButton.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;


procedure TsDBSpeedButton.EditingChanged;
begin
  if not (csLoading in ComponentState) then
    DoEditingChanged(Self, FDataLink, FBtnFunction);
end;


function TsDBSpeedButton.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;


function TsDBSpeedButton.GetImages: TCustomImageList;
var
  res: TCustomImageList;
begin
  res := inherited GetImages;
  if (res = nil) and (Glyph.Empty) then // If glyph is not defined
    Result := acCharImages
  else
    Result := res;
end;


procedure TsDBSpeedButton.SetBtnFunction(const Value: TNavigateBtn);
begin
  if Value <> FBtnFunction then
    FBtnFunction := Value;
end;


procedure TsDBSpeedButton.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDataLink.DataSource then
    FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;

  if Value <> nil then
    Value.FreeNotification(Self);
end;


procedure TsDBBitBtn.ActiveChanged;
begin
  if not FDataLink.Active then
    Enabled := False
  else begin
    DataSetChanged;
    EditingChanged;
  end;
end;


procedure TsDBBitBtn.Click;
var
  AllowAction: Boolean;
begin
  inherited;
  AllowAction := True;
  if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
    FBeforeAction(AllowAction, Self);

  if (DataSource <> nil) and (DataSource.State <> dsInactive) and AllowAction then
    DataSetFunction(DataSource.DataSet, FBtnFunction, FConfirmDelete);

  if Assigned(FAfterAction) then
    FAfterAction(Self);
end;


constructor TsDBBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TsNavButtonDataLink.Create(Self);
  FConfirmDelete := True;
  FColored := False;
  SetBtnFunction(nbFirst);
  Enabled := False;
end;


function TsDBBitBtn.CurrentImageIndex: integer;
begin
  if (inherited GetImages = nil) and Glyph.Empty then begin
{    if Colored then
      GlyphColorTone := DBGlyphColor(SkinData, FBtnFunction, CurrentState)
    else
      GlyphColorTone := clNone;
}
    Result := DBGlyphIndex(FBtnFunction)
  end
  else
    Result := inherited CurrentImageIndex;
end;


procedure TsDBBitBtn.DataSetChanged;
begin
  if not (csLoading in ComponentState) then
    DoDataSetChanged(Self, FDataLink, FBtnFunction);
end;


destructor TsDBBitBtn.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;


procedure TsDBBitBtn.EditingChanged;
begin
  if not (csLoading in ComponentState) then
    DoEditingChanged(Self, FDataLink, FBtnFunction);
end;


function TsDBBitBtn.GetColorTone: TColor;
begin
  if Colored then
    Result := DBGlyphColor(SkinData, FBtnFunction, CurrentState)
  else
    Result := inherited GetColorTone;
end;


function TsDBBitBtn.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;


function TsDBBitBtn.GetImages: TCustomImageList;
var
  res: TCustomImageList;
begin
  res := inherited GetImages;
  if (res = nil) and (Glyph.Empty) then // If glyph is not defined
    Result := acCharImages
  else
    Result := res;
end;


procedure TsDBBitBtn.SetBtnFunction(const Value: TNavigateBtn);
begin
  if Value <> FBtnFunction then
    FBtnFunction := Value;
end;


procedure TsDBBitBtn.SetColored(const Value: boolean);
begin
  if FColored <> Value then begin
    FColored := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsDBBitBtn.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDataLink.DataSource then
    FDataLink.DataSource := Value;

  if not (csLoading in ComponentState) then
    ActiveChanged;

  if Value <> nil then
    Value.FreeNotification(Self);
end;

end.
