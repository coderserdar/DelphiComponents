{***************************************************************}
{                                                               }
{    mailto:drigaylo@isa.ru                                     }
{                                                               }
{    Copyright (c) 2000-2003 Drigaylo S.                        }
{ ------------------------------------------------------------- }
{            home page      : http://VisulDesigner.narod.ru/    }
{ ------------------------------------------------------------- }
{***************************************************************}
unit UOLEActn;
{BLOB - поле не должно нигде показываться - иначе записывается всыкая муть}
interface

uses Windows, Classes, Controls, Forms, olectnrs, ActnList, DB, dbctrls,
     DBActnLnk, DBActnOpns;

const
   cMenuCount = 4;  {количество добавляемых пунктов меню}


Type

 TsibDBOLEAction = class (TAction)
  private
    FOLEFormStyle: TFormStyle;
    FOLEWindowSate: TWindowState;
    FNeedSave : boolean;
    FImageList: TImageList;
    FActionLink: TDBActionLinks; {Если нужно сохранять}
    FOpenDataSets: TDBActionOpens;

    procedure SetDataSource(const Value: TDataSource);
    procedure SetFieldName(const Value: string);
    function GetDataSource:TDataSource;
    function GetFieldName: string;
    procedure SetOLEFormStyle(const Value: TFormStyle);
    procedure SetOLEWindowSate(const Value: TWindowState);
    procedure CollectForm;
  private
    Server,
    ServerDefaultExt,
    CLSID       : string;
    procedure GetCLSIDParam;
  protected
    DL : TFieldDataLink;
    Form:TForm;
    OLEContainer:TOleContainer;
    FProcs : array [1..cMenuCount-1] of TNotifyEvent;
    function Execute:boolean;override;
    procedure FormShow(Sender:TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuProc1(Sender : TObject);
    procedure MenuProc2(Sender : TObject);
    procedure MenuProc3(Sender : TObject);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    function ExecuteSvr(const CommandLine,  CommandParams: string) : integer;
  published
    property DataSource:TDataSource read GetDataSource write SetDataSource;
    property FieldName:string read GetFieldName write SetFieldName;
    property OLEFormStyle:TFormStyle read FOLEFormStyle write SetOLEFormStyle default fsStayOnTop;
    property OLEWindowSate:TWindowState read FOLEWindowSate write SetOLEWindowSate;
    property ImageList:TImageList read FImageList write FImageList;
    property ActionDBLink:TDBActionLinks read FActionLink write FActionLink;
    property NeedOpenDataSets:TDBActionOpens read FOpenDataSets write FOpenDataSets;
 end;


implementation

uses Menus, Dialogs, Registry
{$IFDEF VER150}
, Variants
{$ENDIF}
{$IFDEF VER170}
, Variants
{$ENDIF}
{$IFDEF VER180}
, Variants
{$ENDIF}
;

const
   cMenuCaption : array [0.. cMenuCount-1] of string=(
   'Файл',
   'Удалить документ',
   'Сохранить как...',
   'Открыть для печати'
   );

   cEraseObject = 'После данного действия восстановить объект будет невозможно!'
   +#13#10+'Продолжить ?';
   cSaveObject = 'Сохранить (перезаписать) объект?';


{ TsibDBOLEAction }

procedure TsibDBOLEAction.CollectForm;
var M,M1 : TMenuItem;
    i : integer;
begin
  Form := TForm.Create(Owner);
  Form.FormStyle := FOLEFormStyle;
  Form.WindowState := FOLEWindowSate;
  Form.Position := poScreenCenter;
  Form.OnShow := FormShow;
  Form.OnCloseQuery := FormCloseQuery;
  Form.Menu := TMainMenu.Create(Form);
  //Form.Menu.Parent := Form;
  OLEContainer := TOleContainer.Create(Form);
  OLEContainer.Parent := Form;
  OLEContainer.Align := alClient;

  M1 := TMenuItem.Create(Self);
  M1.Caption := cMenuCaption[0];
  Form.Menu.Items.Add(M1);

  for i:=1 to  cMenuCount-1 do
  try
     M := TMenuItem.Create(Self);
     M.Caption := cMenuCaption[i];
     M.OnClick := FProcs[i];
  finally
     M1.Add(M);
  end;

end;

constructor TsibDBOLEAction.Create(AOwner: TComponent);
begin
  FProcs[1] := MenuProc1;
  FProcs[2] := MenuProc2;
  FProcs[3] := MenuProc3;

  inherited;
  DL := TFieldDataLink.Create;
  FOLEWindowSate := wsMaximized;
  FOLEFormStyle := fsStayOnTop;
  FActionLink := TDBActionLinks.Create(Self, TDBActionLink);
  FOpenDataSets := TDBActionOpens.Create(Self, TDBActionOpen);
end;

destructor TsibDBOLEAction.Destroy;
begin
 FActionLink.Free;
 DL.Free;
 FOpenDataSets.Free;
 inherited;
end;

function TsibDBOLEAction.Execute;
var
   S : TStream;
begin
 FActionLink.DataChange;
 if DL.Field = nil then
    Exit;
 try
  {Создаем форму }
  CollectForm;
  FNeedSave := true;
  {чтение из базы}
  if DL.Field.Value <> Null then
  if DL.Field.Value <> '' then
  try
    try
       S := DL.DataSet.CreateBlobStream(DL.Field, bmReadWrite);
       OLEContainer.LoadFromStream(S);
    except
    end;
  finally
    S.Free;
  end;

  Result := true;
  if OLEContainer.State = osEmpty then
     Result := OLEContainer.InsertObjectDialog;
  Caption := OLEContainer.OleClassName;

   GetCLSIDParam;

  if Result then
     Result := Form.ShowModal = 1;

  {запись в базу}
  if OLEContainer.Modified then
  if FNeedSave then
    try
      try
         DL.DataSet.Edit;
         S := DL.DataSet.CreateBlobStream(DL.Field, bmReadWrite);
         OLEContainer.SaveToStream(S);
         DL.DataSet.Post;
      except
      end;
    finally
      S.Free;
    end;
 finally
   Form.Free;
 end;
 FOpenDataSets.OpenDataSets;
end;

procedure TsibDBOLEAction.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if FNeedSave then {если она не истина, то значит объект очищают}
   FNeedSave := {Application.}MessageBox(Form.Handle, PChar(cSaveObject),PChar(TForm(Owner).Caption), MB_OKCANCEL) = IDOK;
end;

procedure TsibDBOLEAction.FormShow(Sender: TObject);
begin
  if OLEContainer.State <> osEmpty then
     OLEContainer.DoVerb(ovShow);
end;

function TsibDBOLEAction.GetDataSource: TDataSource;
begin
  Result := DL.DataSource;
end;

procedure TsibDBOLEAction.GetCLSIDParam;
var R : TRegistry;
begin
    R :=TRegistry.Create;
    try
      R.RootKey := HKEY_CLASSES_ROOT;
      R.OpenKey(OLEContainer.OleClassName+'\CLSID',False);
      CLSID := R.ReadString('');
      R.CloseKey;
      R.OpenKeyReadOnly('CLSID\'+CLSID+'\LocalServer32');
      Server := R.ReadString('');
      R.CloseKey;
      R.OpenKeyReadOnly('CLSID\'+CLSID+'\DefaultExtension');
      ServerDefaultExt := R.ReadString('');
      if pos(',', ServerDefaultExt) > 0 then
         ServerDefaultExt := Copy(ServerDefaultExt, 1, pos(',', ServerDefaultExt)-1);
    finally
      R.Free;
    end;
end;

function TsibDBOLEAction.GetFieldName: string;
begin
  Result := DL.FieldName;
end;

procedure TsibDBOLEAction.MenuProc1(Sender: TObject);
begin
  if  MessageBox(Form.Handle, PChar(cEraseObject),PChar(TForm(Owner).Caption), MB_OKCANCEL) = IDOK then
  try
     FNeedSave := false;
     if not(DL.Field.DataSet.State in [dsEdit,dsInsert]) then
        DL.Field.DataSet.Edit;
     if DL.Field.DataSet.State in [dsEdit,dsInsert] then
        DL.Field.Value := Null;
  finally
     Form.Close; {если это сделать раньше, акция выполнится}
  end;
end;

procedure TsibDBOLEAction.MenuProc2(Sender: TObject);
begin
  //SendMessage(Form.Handle, $0010{WM_Close} ,0 ,0);
  with TSaveDialog.Create(Self) do
  try
    //OLEContainer.DoVerb(ovUIActivate);
    Form.Hide;
    DefaultExt := ServerDefaultExt;
    if Execute then
       OLEContainer.SaveAsDocument(FileName);
  finally
    //OLEContainer.DoVerb(ovShow);
    Form.Show;
    Free;
  end;
end;

procedure TsibDBOLEAction.SetDataSource(const Value: TDataSource);
begin
  DL.DataSource := Value;
end;

procedure TsibDBOLEAction.SetFieldName(const Value: string);
begin
  DL.FieldName := Value;
end;

procedure TsibDBOLEAction.SetOLEFormStyle(const Value: TFormStyle);
begin
  FOLEFormStyle := Value;
end;

procedure TsibDBOLEAction.SetOLEWindowSate(const Value: TWindowState);
begin
  FOLEWindowSate := Value;
end;

function TsibDBOLEAction.ExecuteSvr(const CommandLine,  CommandParams: string) : integer;
var
  R : boolean;
  ProcessInformation : TProcessInformation;
  StartupInfo : TStartupInfo;
  ExCode : cardinal;
begin
  Result := 0;
  FillChar(StartupInfo, sizeof(TStartupInfo), 0);
  with StartupInfo do begin
    cb := sizeof(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_SHOW;
  end;

  R := CreateProcess(
    nil,//PChar(CommandLine), // pointer to name of executable module
    PChar(CommandLine+' '+CommandParams ), // pointer to command line string
    nil,// pointer to process security attributes
    nil,// pointer to thread security attributes
    false, // handle inheritance flag
    0, // creation flags
    nil,// pointer to new environment block
    nil,//PChar(WorkingDirectory), // pointer to current directory name
    StartupInfo, // pointer to STARTUPINFO
    ProcessInformation // pointer to PROCESS_INFORMATION
   );
  if R then
    while (GetExitCodeProcess(ProcessInformation.hProcess, ExCode) and
          (ExCode = STILL_ACTIVE))
    do Result := 0
  else
    Result := GetLastError;

end;


procedure TsibDBOLEAction.MenuProc3(Sender: TObject);
var FN : string;
begin
  try
      Form.Hide;
      FN := '$$0123'+ServerDefaultExt;
      OLEContainer.SaveAsDocument(FN);
      ExecuteSvr(Server,FN);
  finally
    //OLEContainer.DoVerb(ovShow);
    FNeedSave := false;
//    DeleteFile(PChar(FN));
    Form.Close;
  end;
end;

end.
