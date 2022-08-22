
{**********************************************************}
{                                                          }
{  Cipher 1.0                                              }
{  ---- Sample for Using TinyDB                            }
{                                                          }
{  Author: DayDream Studio                                 }
{  Email: webmaster@tinydb.com                             }
{  URL: http://www.TinyDB.com                              }
{                                                          }
{**********************************************************}

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, ToolWin,
  Menus, Db, ActnList, ImgList, ExtCtrls,
  PwdDataFrm, TinyDB;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ToolBar: TToolBar;
    ListView: TListView;
    StatusBar: TStatusBar;
    ToolButton1: TToolButton;
    HelpMenu: TMenuItem;
    HelpAboutItem: TMenuItem;
    FileExitItem: TMenuItem;
    TinyDatabase: TTinyDatabase;
    TinyTable: TTinyTable;
    ActionList: TActionList;
    NewAction: TAction;
    DeleteAction: TAction;
    ModifyAction: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ImageList: TImageList;
    FileN1Item: TMenuItem;
    FileNewItem: TMenuItem;
    FileDeleteItem: TMenuItem;
    FileModifyItem: TMenuItem;
    Panel1: TPanel;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ChgPwdAction: TAction;
    FileChgPwdItem: TMenuItem;
    FileN2Item: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure ModifyActionExecute(Sender: TObject);
    procedure ChgPwdActionExecute(Sender: TObject);
    procedure FileExitItemClick(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
  private
    { Private declarations }
    function GetDBFileName: string;
    function CreateDatabase(Password: string): Boolean;
    procedure SavePwdData(Data: TPwdDataFormData);
    procedure FillListView;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses StrRes, SetPwdFrm, InputFrm, AboutFrm;

function TMainForm.GetDBFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'Cipher.dat';
end;

function TMainForm.CreateDatabase(Password: string): Boolean;
var
  DBFileName: string;
begin
  Result := True;
  DBFileName := GetDBFileName;
  try
    TinyDatabase.CreateDatabase(DBFileName, False, clNormal, 'ZLIB', True, 'Blowfish', Password, True);
    TinyDatabase.DatabaseName := DBFileName;
    TinyDatabase.Password := Password;

    TinyDatabase.CreateTable('Cipher', [
      FieldItem('ID', ftAutoInc),
      FieldItem('Password', ftString, 64),
      FieldItem('Note', ftMemo)
      ] );

    TinyDatabase.CreateIndex('Cipher', 'ByPassword', [], ['Password']);
  except
    DeleteFile(DBFileName);
    Result := False;
  end;
end;

procedure TMainForm.SavePwdData(Data: TPwdDataFormData);
begin
  TinyTable.AppendRecord([0, Data.Password, Data.Note]);
end;

procedure TMainForm.FillListView;
var
  I: Integer;
  ListItem: TListItem;
begin
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  TinyTable.First;
  for I := 0 to TinyTable.RecordCount - 1 do
  begin
    ListItem := ListView.Items.Add;
    ListItem.Caption := TinyTable.FieldByName('Password').AsString;
    ListItem.SubItems.Add(TinyTable.FieldByName('Note').AsString);
    ListItem.ImageIndex := 4;
    ListItem.Data := Pointer(TinyTable.FieldByName('ID').AsInteger);
    TinyTable.Next;
  end;
  ListView.Items.EndUpdate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  DBFileName: string;
  Password: string;
  Tip: string;
begin
  DBFileName := GetDBFileName;
  if not FileExists(DBFileName) then
  begin
    Tip := SStartupTip;
    if not ShowSetPwdForm(Password, Tip, SInputPassword) then
    begin
      Position := poDesigned;
      Left := -1000;
      Application.Terminate;
      Exit;
    end;
    if not CreateDatabase(Password) then
    begin
      MessageBox(Application.Handle, PChar(SFailToCreateDb), PChar(Application.Title), 16);
      Application.Terminate;
    end;
  end else
  begin
    TinyDatabase.Close;
    TinyDatabase.DatabaseName := DBFileName;
    TinyDatabase.Open;
    if ShowInputForm(Password, Self.Caption, SInputPassword, True) then
    begin
      TinyDatabase.Password := Password;
      if not TinyDatabase.CanAccess then
      begin
        Application.MessageBox(PChar(SPasswordWrong), PChar(Application.Title), 48);
        Application.Terminate;
      end;
    end else
    begin
      Application.Terminate;
      Exit;
    end;
  end;

  TinyTable.DatabaseName := DBFileName;
  TinyTable.TableName := 'Cipher';
  TinyTable.Password := Password;
  TinyTable.Open;
  FillListView;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TinyTable.Close;
end;

procedure TMainForm.NewActionExecute(Sender: TObject);
var
  Data: TPwdDataFormData;
begin
  if ShowPwdDataForm(Data) then
  begin
    SavePwdData(Data);
    FillListView;
  end;
end;

procedure TMainForm.DeleteActionExecute(Sender: TObject);
var
  ID, R: Integer;
begin
  if ListView.Selected = nil then Exit;

  R := MessageBox(Application.Handle, PChar(SQueryDeletePassword), PChar(Application.Title), 36);
  if R = ID_NO then Exit;

  ID := Integer(ListView.Selected.Data);
  if TinyTable.FindKey([ID]) then
  begin
    TinyTable.Delete;
    ListView.Selected.Delete;
  end;
end;

procedure TMainForm.ModifyActionExecute(Sender: TObject);
var
  ID: Integer;
  Data: TPwdDataFormData;
  ListItem: TListItem;
begin
  if ListView.Selected = nil then Exit;
  ListItem := ListView.Selected;
  ID := Integer(ListItem.Data);
  if TinyTable.FindKey([ID]) then
  begin
    Data.Password := TinyTable.FieldByName('Password').AsString;
    Data.Note := TinyTable.FieldByName('Note').AsString;
    if ShowPwdDataForm(Data) then
    begin
      TinyTable.Edit;
      TinyTable.FieldByName('Password').AsString := Data.Password;
      TinyTable.FieldByName('Note').AsString := Data.Note;
      TinyTable.Post;

      ListItem.Caption := Data.Password;
      ListItem.SubItems[0] := Data.Note;
    end;
  end;
end;

procedure TMainForm.ChgPwdActionExecute(Sender: TObject);
var
  Password, Tip: string;
begin
  Tip := SChangePwdTip;
  if ShowSetPwdForm(Password, Tip, SChangePassord) then
  begin
    TinyTable.Close;
    if TinyDatabase.ChangePassword(Password, True) then
    begin
      MessageBox(Handle, PChar(SChgPwdSucc), PChar(Application.Title), 48);
      TinyTable.Password := Password;
    end;
    TinyTable.Open;
  end;
end;

procedure TMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HelpAboutItemClick(Sender: TObject);
begin
  ShowAboutForm;
end;

end.
