unit DBQryActn;
{***************************************************************}
{                                                               }
{ Component for  executing SQL-query from action                }
{    mailto:drigaylo@isa.ru                                     }
{                                                               }
{    Copyright (c) 2000-2003 Drigaylo S.                        }
{ ------------------------------------------------------------- }
{            home page      : http://VisulDesigner.narod.ru/    }
{ ------------------------------------------------------------- }
{***************************************************************}
interface

uses ActnList, Classes, Forms, Windows, controls, Db, DBActnLnk, DBActnFlds, DBActnOpns;

type
  TQueryAction = class(TCustomAction)
  private
    FDS : TDataSet;
    FRes : String;

    FShow : boolean;
    //FReopenMaster : boolean;
    FImageList: TImageList;
    FActionLink: TDBActionLinks;
    FOpenDataSets: TDBActionOpens;
    FClassName: string;
    FParams: TStrings;
    Strg : string;
    FOA: TNotifyEvent;
    FOB: TNotifyEvent;
    function GetParams: TStrings;
    procedure SetParams(const Value: TStrings);
  protected
    procedure DoOnExecute(Sender: TObject);
  public
    function ExecDataSet: TDataSet;
    function Execute: boolean; override;
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    property DataSet: TDataSet read FDS;
  published
    property DataSetClassName: string read FClassName write FClassName;
    property DataSetParams: TStrings read GetParams write SetParams;
    property Result: string read FRes;
    property ShowResultMessage: boolean read FShow write FShow default false;
    property ImageList: TImageList read FImageList write FImageList;
    property ActionDBLink: TDBActionLinks read FActionLink write FActionLink;
    property NeedOpen: TDBActionOpens read FOpenDataSets write FOpenDataSets;
    property OnBeforeExecute: TNotifyEvent read FOB write FOB;
    property OnAfterExecute: TNotifyEvent read FOA write FOA;
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

uses Sysutils, Dialogs
{$IFNDEF VER130}
, Variants
{$ENDIF}
;


{ TQueryAction }

constructor TQueryAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActionLink := TDBActionLinks.Create(Self, TDBActionLink);
  FOpenDataSets := TDBActionOpens.Create(Self, TDBActionOpen);
  FParams := TStringList.Create;
  inherited OnExecute := DoOnExecute;
end;

destructor TQueryAction.Destroy;
begin
  FParams.Free;
  FActionLink.Free;
  FOpenDataSets.Free;
  inherited;
end;

procedure TQueryAction.DoOnExecute(Sender: TObject);
begin
//terminator
end;

function TQueryAction.ExecDataSet: TDataSet;
var P : TPersistentClass;
    DS: TComponent;
    S : TStringStream;
    M : TMemoryStream;
begin
  P := GetClass(FClassName);

  if P = nil then
     raise Exception.CreateFmt('DB Class "%s" not Registered!'+#13#10
     +'Create before execute action %s do: "RegisterClass(%s);"',
     [FClassName, Self.Name, FClassName]);
  try
    Strg := 'object DB' + Self.Name + IntToStr(GetTickCount) + ':' + FClassName +#13#10
         + FParams.Text + #13#10
         +'end'+#13#10;
    S := TStringStream.Create(Strg);

    M := TMemoryStream.Create;
    S.Position := 0;
    try
      ObjectTextToBinary(S, M);
    except on E: Exception do Raise Exception.Create('Error in converting DataSetParams:'
      + E.Message);
    end;
    S.Position := 0;
    M.Seek(0, soFromBeginning);
    DS := M.ReadComponent(nil);
    Result := TDataSet(DS);
  finally
    S.Free;
    M.Free;
  end;
end;

function TQueryAction.Execute: boolean;
var i : integer;
begin
  Result := true;
  begin
     Screen.Cursor := crHourGlass;
     try
        if Assigned(FOB) then FOB(Self);
        FDS := ExecDataSet;
        if FDS <> nil then
        begin
           FDS.Open;
          FRes := 'Выполнено!';
          IF FDS.FieldCount >0 then
          if FDS.Fields[0].Value<>Null then
             FRes := FDS.Fields[0].Value;
          if Assigned(FOA) then FOA(Self);
          if FShow then
            ShowMessage( FRes);
          FDS.Close;
        end;
     finally
        if FDS <> nil then
           FDS.Free;
        Screen.Cursor := crDefault;
     end;

     for i:= 0 to FOpenDataSets.Count -1 do
        FOpenDataSets[i].SaveDataSet;
     FActionLink.DataChange;
     FOpenDataSets.OpenDataSets;

     for i:= 0 to FOpenDataSets.Count -1 do
        FOpenDataSets[i].RestoreDataSet;
  end;
end;

function TQueryAction.GetParams: TStrings;
begin
   Result := FParams;
end;

procedure TQueryAction.SetParams(const Value: TStrings);
begin
  if DataSetParams.Text <> Value.Text then
  begin
    DataSetParams.BeginUpdate;
    try
      DataSetParams.Assign(Value);
    finally
      DataSetParams.EndUpdate;
    end;
  end;
end;

end.
