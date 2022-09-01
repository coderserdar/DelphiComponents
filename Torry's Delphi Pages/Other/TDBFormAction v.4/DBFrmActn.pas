{***************************************************************}
{                                                               }
{ Component for executing interacts between forms               }
{      mail over http http://visualdesigner.fatal.ru            }
{    http://visualdesigner.fatal.ru/                            }
{    Copyright (c) 2000-2003 Pfaffenrot S.                      }
{ ------------------------------------------------------------- }
{            home page      : http://VisulDesigner.fatal.ru/    }
{ ------------------------------------------------------------- }
{***************************************************************}
unit DBFrmActn;

interface

uses DB, Classes, Controls, ActnList, Forms;

type
  TDBFormAction = class;
  TDBCol = class;
  TDataSetDir = (dsdGet, dsdSet);
  TDBNode = class(TCollectionItem)
  private
    FSetDS: TComponentName;
    FGetF: TComponentName;
    FSetF: TComponentName;
    FGetDS: TComponentName;
    FGetIndex, FSetIndex: integer;
    FDBCol : TDBCol;
    FAction: TDBFormAction;
    FGetField, FSetField: TField;
    FSetPost: boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure FindDataSets;
    procedure FindFields;
  published
    property GetDataSet: TComponentName read FGetDS write FGetDS;
    property SetDataSet: TComponentName read FSetDS write FSetDS;
    property GetFieldName: TComponentName read FGetF write FGetF;
    property SetFieldName: TComponentName read FSetF write FSetF;
    property SetDataSetPost: boolean read FSetPost write FSetPost;
  end;

  TDBCol = class (TOwnedCollection)
  private
    FSetDS, FGetDS: TList;
    function GetItem(Index: Integer): TDBNode;
    procedure SetItem(Index: Integer; const Value: TDBNode);
  protected
    function GetRefCount(const AList: TDataSetDir; const AIndex: integer): cardinal;
  public
    constructor Create(AOwner: TPersistent); 
    destructor Destroy; override;
    property Items[Index: Integer]: TDBNode read GetItem write SetItem; default;
    function Add: TDBNode;
    function GetLocateParams(const ADir: TDataSetDir;const AIndex: integer; var AKeys: string; var AFiledsValues: Variant): TDataSet;
    function GetDataSetByName(AComponentName: TComponentName): TComponent;
  end;

  TODSCol = class;
  TODSItem = class (TCollectionItem)
  private
    FDSN: TComponentName;
    FODSIndex: integer;
  public
    constructor Create(Collection: TCollection); override;
  published
    property DataSetName: TComponentName read FDSN write FDSN;
  end;
  TODSCol = class(TOwnedCollection)
  private
    FDSList: TList;
    FAction: TDBFormAction;
    function GetItem(Index: integer): TODSItem;
    procedure SetItem(Index: integer; const Value: TODSItem);
  public
    function Add: TODSItem;
    property Items[Index: integer]: TODSItem read GetItem write SetItem; default;
    constructor Create(AOwner: TPersistent); 
    destructor Destroy; override;
    procedure FindDataSets;
  end;

  TOnComponentFind = procedure(Sender: TObject; AComponentName: TComponentName; var AComponent: TComponent) of object;
  TLoadFormEvent = procedure(Sender: TObject; AFormName: string; var AForm: TForm) of object;
  TDBFormActionStatus = (fasLocate, fasReturn);
  TActionDebugInfo = procedure(Sender: TObject; ADetailForm: TForm;
      ADataSet: TDataSet; const AStatus: TDBFormActionStatus) of Object;
  TOnFormShow = procedure (Sender: TObject; AForm: TForm) of object;

  TDBFormAction = class(TCustomAction)
  private
    FForm: TForm;
    FRes: TDBCol;
    FLoc: TDBCol;
    FOCF: TOnComponentFind;
    FOLF: TLoadFormEvent;
    FDestFN: TComponentName;
    FODSCol: TODSCol;
    FOFS: TOnFormShow;
    FCloseForm: boolean;
    FModalResult: TModalResult;
    OldFormCloseQuery: TCloseQueryEvent;
    FOBE: TNotifyEvent;
    FOAE: TNotifyEvent;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Form: TForm read FForm write FForm;
    function Execute: boolean; override;
  protected
    function FindDataSet(ADataSetName: TComponentName): TDataSet;
  published
    property DestFormName: TComponentName read FDestFN write  FDestFN;
    property OnComponetFind: TOnComponentFind read FOCF write FOCF;
    property OnLoadForm: TLoadFormEvent read FOLF write FOLF;
    property Locating: TDBCol read FLoc write FLoc;
    property Restoring: TDBCol read FRes write FRes;
    property OpenDataSets: TODSCol read FODSCol write FODSCol;
    property OnFormShow: TOnFormShow read FOFS write FOFS;
    property OnBeforeExecute: TNotifyEvent read FOBE write FOBE;
    property OnAfterExecute: TNotifyEvent read FOAE write FOAE;
  published
    property Caption;
    property Checked;
    property Enabled;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property Visible;
    property OnHint;
    property OnUpdate;   
  end;

implementation

uses SysUtils, Variants;

resourcestring
  cNotFoundDataSet = 'Not found dataset ';

const
  TModifState = [dsEdit, dsInsert];

{ TDBNode }

constructor TDBNode.Create(Collection: TCollection);
begin
  inherited;
  FGetIndex := -1;
  FSetIndex := -1;
  SetLength(FGetF, 0);
  SetLength(FGetDS, 0);
  SetLength(FSetDS, 0);
  SetLength(FSetF, 0);
  FDBCol := TDBCol(Collection);
  FAction := TDBFormAction(FDBCol.GetOwner);
end;

destructor TDBNode.Destroy;
begin
  if FSetIndex > -1 then
  if FDBCol.GetRefCount(dsdSet, FSetIndex) = 1 then
    FDBCol.FSetDS.Delete(FSetIndex);
  if FGetIndex > -1 then
  if FDBCol.GetRefCount(dsdGet, FGetIndex) = 1 then
    FDBCol.FGetDS.Delete(FGetIndex);
  inherited;
end;

procedure TDBNode.FindDataSets;
var
  S, D : TDataSet;
begin                        
  S := FAction.FindDataSet(FGetDS);

  D := FAction.FindDataSet(FSetDS);

  if (S = nil) then
    raise Exception.Create(cNotFoundDataSet + FGetDS);
  if (D = nil) then
    raise Exception.Create(cNotFoundDataSet + FSetDS);
  FGetIndex := FDBCol.FGetDS.IndexOf(S);
  if FGetIndex < 0 then
    FGetIndex := FDBCol.FGetDS.Add(S);
  FSetIndex := FDBCol.FSetDS.IndexOf(D);
  if FSetIndex < 0 then
    FSetIndex := FDBCol.FSetDS.Add(D);
end;

procedure TDBNode.FindFields;
begin
  if FGetIndex > -1 then
    FGetField := TDataSet(FDBCol.FGetDS[FGetIndex]).FieldByName(FGetF);
  if FSetIndex > -1 then
    FSetField := TDataSet(FDBCol.FSetDS[FSetIndex]).FieldByName(FSetF);
end;

{ TDBCol }

function TDBCol.Add: TDBNode;
begin
  Result := TDBNode(inherited Add);
end;

constructor TDBCol.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TDBNode);
  FSetDS := TList.Create;
  FGetDS := TList.Create;
end;

destructor TDBCol.Destroy;
begin
  Clear;
  FGetDS.Free;
  FSetDS.Free;
  inherited;
end;

function TDBCol.GetDataSetByName(AComponentName: TComponentName): TComponent;
var i : integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[i].FGetIndex > -1 then
    begin
      if Items[i].FGetDS = AComponentName then
      begin
        Result := TComponent(FGetDS[Items[i].FGetIndex]);
        Exit;
      end;
    end
    else
    if Items[i].FSetIndex > -1 then
    begin
      if Items[i].FSetDS = AComponentName then
      begin
        Result := TComponent(FSetDS[Items[i].FSetIndex]);
        Exit;
      end;
    end;
end;

function TDBCol.GetItem(Index: Integer): TDBNode;
begin
  Result := TDBNode(inherited Items[Index]);
end;

function TDBCol.GetLocateParams(const ADir: TDataSetDir; const AIndex: integer;
  var AKeys: string; var AFiledsValues: Variant): TDataSet;
var i, j, Cnt : integer; V : Variant;
begin
  Result := nil;
  Cnt := 0; SetLength(AKeys, 0);
  for i := 0 to Count - 1 do
  case ADir of
    dsdGet:if Items[i].FGetIndex = AIndex then
           if not Items[i].FSetField.IsNull then           
           begin
             inc(Cnt);
             AKeys := AKeys +';' + Items[i].FGetF;
             V := VarArrayCreate([1, Cnt], varVariant);
             for j := 1 to Cnt - 1 do
               V[j] := AFiledsValues[j];
             V[Cnt] := Items[i].FGetField.Value;
             AFiledsValues := V;
           end;
    dsdSet:if Items[i].FSetIndex = AIndex then
           if not Items[i].FSetField.IsNull then           
           begin
             Result := TDataSet(FSetDS[Items[i].FSetIndex]);
             inc(Cnt);
             AKeys := AKeys +';' + Items[i].FSetF;
             V := VarArrayCreate([1, Cnt], varVariant);
             for j := 1 to Cnt - 1 do
               V[j] := AFiledsValues[j];
             V[Cnt] := Items[i].FGetField.Value;
             AFiledsValues := V;
           end;
  end;
  System.delete(AKeys, 1, 1);
  if Cnt = 1 then
    AFiledsValues := AFiledsValues[Cnt];  
end;

function TDBCol.GetRefCount(const AList: TDataSetDir; const AIndex: integer): cardinal;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  case AList of
    dsdGet: if Items[i].FGetIndex = AIndex then inc(Result);
    dsdSet: if Items[i].FSetIndex = AIndex then inc(Result);
  end;
end;

procedure TDBCol.SetItem(Index: Integer; const Value: TDBNode);
begin
end;

{ TDBFormAction }

constructor TDBFormAction.Create(AOwner: TComponent);
begin
  inherited;
  FLoc := TDBCol.Create(Self);
  FRes := TDBCol.Create(Self);
  FODSCol := TODSCol.Create(Self);
end;

destructor TDBFormAction.Destroy;
begin
  FLoc.Free;
  FRes.Free;
  FODSCol.Free;
  inherited;
end;

function TDBFormAction.Execute: boolean;
var
  Cur: TCursor;
  i,j : integer;
  AKey: string;
  V : Variant;
  DS, GetDS : TDataSet;
begin
  Enabled := False;
  Cur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Result := False;
  try
    if Assigned(FOBE) then
      FOBE(Self);
    if Assigned(FOLF) then
      FOLF(Self, FDestFN, FForm);

    for i := 0 to FLoc.Count - 1 do
      FLoc.Items[i].FindDataSets;
    for i := 0 to FRes.Count - 1 do
      FRes.Items[i].FindDataSets;

    {SetDataSource}
    {OpenDataSets}
    for I := 0 to FLoc.FGetDS.Count - 1 do
    begin
      DS := TDataSet(FLoc.FGetDS[i]);
      if not DS.Active then
        DS.Open;
    end;
    for I := 0 to FLoc.Count - 1 do
      FLoc[i].FGetField := TDataSet(FLoc.FGetDS[FLoc[i].FGetIndex]).FieldByName(FLoc[i].FGetF);
    for I := 0 to FLoc.FSetDS.Count - 1 do
    begin
      DS := TDataSet(FLoc.FSetDS[i]);
      if not DS.Active then
        DS.Open;
    end;
    for I := 0 to FLoc.Count - 1 do
      FLoc[i].FSetField := TDataSet(FLoc.FSetDS[FLoc[i].FSetIndex]).FieldByName(FLoc[i].FSetF);
    for I := 0 to FRes.FGetDS.Count - 1 do
    begin
      DS := TDataSet(FRes.FGetDS[i]);
      if not DS.Active then
        DS.Open;
    end;
    for I := 0 to FRes.Count - 1 do
      FRes[i].FGetField := TDataSet(FRes.FGetDS[FRes[i].FGetIndex]).FieldByName(FRes[i].FGetF);
    for I := 0 to FRes.FSetDS.Count - 1 do
    begin
      DS := TDataSet(FRes.FSetDS[i]);
      if not DS.Active then
        DS.Open;
    end;
    for I := 0 to FRes.Count - 1 do
      FRes[i].FSetField := TDataSet(FRes.FSetDS[FRes[i].FSetIndex]).FieldByName(FRes[i].FSetF);
    {}
    {Locate FLoc}
    for I := 0 to FLoc.FSetDS.Count - 1 do
    begin
      DS := FLoc.GetLocateParams(dsdSet, i, AKey, V);
      if DS <> nil then
        DS.Locate(AKey, V, [loCaseInsensitive]);
    end;
    {}

   {показ}
   Screen.Cursor := Cur;
   if Assigned(FOFS) then
     FOFS(Self, FForm)
   else
   begin
     if FForm.FormStyle  in [fsMDIChild,fsMDIForm] then
     begin
        FCloseForm := False;
        OldFormCloseQuery := FForm.OnCloseQuery;
        FForm.OnCloseQuery := FormCloseQuery;
        FForm.Show;
        repeat
          Application.ProcessMessages;
        until FCloseForm;
        FForm.OnCloseQuery := OldFormCloseQuery;
        Result := FModalResult = 1;
     end
     else
        Result := FForm.ShowModal = 1;
   end;
   Screen.Cursor := crHourGlass;
   {}
   {Set new values}
   if Result then
   begin
     for I := 0 to FRes.Count - 1 do
     begin
       GetDS := TDataSet(FRes.FGetDS[FRes[i].FGetIndex]);
       DS := TDataSet(FRes.FSetDS[FRes[i].FSetIndex]);
       if not (DS.State in TModifState) then
          DS.Edit;
       DS.FieldByName(FRes[i].FSetF).Assign(GetDS.FieldByName(FRes[i].FGetF));
     end;
     {}
     for I := 0 to FRes.Count - 1 do
     if FRes[i].FSetPost then
     begin
       DS := TDataSet(FRes.FSetDS[FRes[i].FSetIndex]);
       if DS.State in TModifState then
       try
         DS.Post;
       except
       end;
     end;
   end;

    if Assigned(FOAE) then
      FOAE(Self);
  finally
    Screen.Cursor := Cur;
    Enabled := True;
  end;
end;

function TDBFormAction.FindDataSet(ADataSetName: TComponentName): TDataSet;
var i : integer;
begin
  Result := nil;
  for I := 0 to FLoc.Count - 1 do
  begin
    if (FLoc[i].FGetDS = ADataSetName)and(FLoc[i].FGetIndex > -1) then
    begin
      Result := TDataSet(FLoc.FGetDS[FLoc[i].FGetIndex]);
      Exit;
    end;
    if (FLoc[i].FSetDS = ADataSetName)and(FLoc[i].FSetIndex > -1) then
    begin
      Result := TDataSet(FLoc.FSetDS[FLoc[i].FSetIndex]);
      Exit;
    end;
  end;
  for I := 0 to FRes.Count - 1 do
  begin
    if (FRes[i].FGetDS = ADataSetName)and(FRes[i].FGetIndex > -1) then
    begin
      Result := TDataSet(FRes.FGetDS[FRes[i].FGetIndex]);
      Exit;
    end;
    if (FRes[i].FSetDS = ADataSetName)and(FRes[i].FSetIndex > -1) then
    begin
      Result := TDataSet(FRes.FSetDS[FRes[i].FSetIndex]);
      Exit;
    end;
  end;
  for I := 0 to FODSCol.Count - 1 do
   if FODSCol[i].FDSN = ADataSetName then
   begin
     Result := TDataSet(FODSCol.FDSList[ FODSCol[i].FODSIndex]);
     Exit;
   end;
  if Assigned(FOCF) then
    FOCF(Self, ADataSetName, TComponent(Result));
end;

procedure TDBFormAction.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FModalResult := TCustomForm(Sender).ModalResult;
  if Assigned(OldFormCloseQuery) then
  try
     OldFormCloseQuery(Sender, CanClose);
  except
  end;
  FCloseForm := CanClose;

//  if TCustomForm(Sender).ModalResult = mrOk then
//  if FReturnFields.Count > 0 then
//       FReturnFields.Save;
end;

{ TODSCol }

function TODSCol.Add: TODSItem;
begin
  Result := TODSItem(inherited Add);
end;

constructor TODSCol.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TODSItem);
  FAction := TDBFormAction(AOwner);
  FDSList := TList.Create;
end;

destructor TODSCol.Destroy;
begin
  FDSList.Free;
  inherited;
end;

procedure TODSCol.FindDataSets;
var i,j : integer;  DS : TDataSet;
begin
  for i := 0 to Count - 1 do
  begin
    DS := FAction.FindDataSet(Items[i].FDSN);
    j := FDSList.IndexOf(DS);
    if j < 0 then
      j := FDSList.Add(DS);
    Items[i].FODSIndex := j;
  end;
end;

function TODSCol.GetItem(Index: integer): TODSItem;
begin
  Result := TODSItem(inherited Items[Index]);
end;

procedure TODSCol.SetItem(Index: integer; const Value: TODSItem);
begin
end;

{ TODSItem }

constructor TODSItem.Create(Collection: TCollection);
begin
  inherited;
  FODSIndex := -1;
end;

end.
