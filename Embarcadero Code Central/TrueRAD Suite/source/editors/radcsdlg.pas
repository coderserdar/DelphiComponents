
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit radcsdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Grids, ToolWin, ImgList, ActnList, radconlist, radp2p,
  rade2p, rade2m, Menus, DsgnIntf;

type
  TradConnectionEditor = class(TForm)
    ilButtons: TImageList;
    tbButtons: TToolBar;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    ToolButton10: TToolButton;
    ActionList1: TActionList;
    cmAddP2P: TAction;
    lvConnections: TListView;
    ilConnectors: TImageList;
    cmAddE2P: TAction;
    cmAddE2M: TAction;
    cmDelete: TAction;
    tbMoveUp: TToolButton;
    tbMoveDown: TToolButton;
    cmMoveUp: TAction;
    cmMoveDown: TAction;
    pmAdd: TPopupMenu;
    miAddP2P: TMenuItem;
    miAddE2P: TMenuItem;
    miAddE2M: TMenuItem;
    PopupMenu: TPopupMenu;
    miDelete: TMenuItem;
    N3: TMenuItem;
    miMoveUp: TMenuItem;
    miMoveDown: TMenuItem;
    miNew: TMenuItem;
    PropertytoProperty1: TMenuItem;
    EventtoProperty1: TMenuItem;
    EventtoMethod1: TMenuItem;
    cmAdd: TAction;
    procedure cmAddP2PExecute(Sender: TObject);
    procedure cmAddE2PExecute(Sender: TObject);
    procedure cmAddE2MExecute(Sender: TObject);
    procedure cmDeleteExecute(Sender: TObject);
    procedure cmMoveUpExecute(Sender: TObject);
    procedure cmMoveDownExecute(Sender: TObject);
    procedure cmDeleteUpdate(Sender: TObject);
    procedure cmMoveUpUpdate(Sender: TObject);
    procedure cmMoveDownUpdate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvConnectionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDesigner: IFormDesigner;
    FComponent: TradConnectionList;
    procedure UpdateListBox;
    procedure Add(Cls: TComponentClass; const Name: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

procedure radShowConnectionsEditor(ADesigner: IDesigner; AComponent: TComponent);

implementation

{$R *.DFM}

uses registry;

var
  ConnectionEditorsList: TList = nil;

procedure radShowConnectionsEditor(ADesigner: IDesigner; AComponent: TComponent);
var
    i: Integer;
    Editor: TradConnectionEditor;
begin
    if ConnectionEditorsList = nil then
        ConnectionEditorsList := TList.Create;

    for i := 0 to ConnectionEditorsList.Count-1 do begin
        Editor := TradConnectionEditor(ConnectionEditorsList[i]);
        with Editor do begin
            if (FDesigner = ADesigner) and (FComponent = AComponent) then begin
                Show;
                BringToFront;
                Exit;
            end;
        end;
    end;

    Editor := TradConnectionEditor.Create(Application);
    with Editor do
    try
        FDesigner := ADesigner as IFormDesigner;
        FComponent := AComponent as TradConnectionList;
        FComponent.FreeNotification(Editor);
        UpdateListbox;
        Show;
    except
        Free;
    end;
end;

procedure TradConnectionEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if (Operation = opRemove) and (AComponent = FComponent) then begin
        FComponent := nil;
        Close;
    end;
end;

procedure TradConnectionEditor.Add(Cls: TComponentClass; const Name: String);
var
    NewCon: TradConnection;
    NewItem: TListItem;
begin
    NewCon := TradConnection(FDesigner.CreateComponent(Cls, nil, 0, 0, 0, 0));
    NewCon.Name := FDesigner.UniqueName(Name);
    NewCon.ConnectionList := FComponent;
    FDesigner.Modified;

    NewItem := lvConnections.Items.Add;
    NewItem.Caption := NewCon.Name;
    NewItem.Data := NewCon;
    if NewCon is TradP2PConnection then
        NewItem.ImageIndex := 0
    else if NewCon is TradE2PConnection then
        NewItem.ImageIndex := 1
    else if NewCon is TradE2MConnection then
        NewItem.ImageIndex := 2;
    NewItem.Selected := True;
    NewItem.Focused := True;
    NewItem.MakeVisible(False);
end;

procedure TradConnectionEditor.UpdateListBox;
var
    i: Integer;
    NewItem: TListItem;
begin
    lvConnections.Items.BeginUpdate;
    lvConnections.Items.Clear;
    try
        for i := 0 to FComponent.ConnectionCount - 1 do begin
            NewItem := lvConnections.Items.Add;
            NewItem.Caption := FComponent[i].Name;
            NewItem.Data := FComponent[i];
            if FComponent[i] is TradP2PConnection then
                NewItem.ImageIndex := 0
            else if FComponent[i] is TradE2PConnection then
                NewItem.ImageIndex := 1
            else if FComponent[i] is TradE2MConnection then
                NewItem.ImageIndex := 2;
        end;
    finally
        lvConnections.Items.EndUpdate;
    end;
end;

procedure TradConnectionEditor.cmAddP2PExecute(Sender: TObject);
begin
    Add(TradP2PConnection, 'P2PConnection');
end;

procedure TradConnectionEditor.cmAddE2PExecute(Sender: TObject);
begin
    Add(TradE2PConnection, 'E2PConnection');
end;

procedure TradConnectionEditor.cmAddE2MExecute(Sender: TObject);
begin
    Add(TradE2MConnection, 'E2MConnection');
end;

procedure TradConnectionEditor.cmDeleteExecute(Sender: TObject);
begin
    if MessageDlg('Delete ' + lvConnections.Selected.Caption +
                  ' connection?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
        FDesigner.DeleteSelection;
        lvConnections.Selected.Delete;
        FDesigner.Modified;
    end;
end;

procedure TradConnectionEditor.cmMoveUpExecute(Sender: TObject);
var
    Connection: TradConnection;
begin
    Connection := TradConnection(lvConnections.Selected.Data);
    Connection.Index := Connection.Index - 1;
    UpdateListBox;
    lvConnections.Items[Connection.Index].Selected := True;
    lvConnections.Items[Connection.Index].Focused := True;
    lvConnections.Items[Connection.Index].MakeVisible(False);
    FDesigner.SelectComponent(FComponent[Connection.Index]);
    FDesigner.Modified;
end;

procedure TradConnectionEditor.cmMoveDownExecute(Sender: TObject);
var
    Connection: TradConnection;
begin
    Connection := TradConnection(lvConnections.Selected.Data);
    Connection.Index := Connection.Index + 1;
    UpdateListBox;
    lvConnections.Items[Connection.Index].Selected := True;
    lvConnections.Items[Connection.Index].Focused := True;
    lvConnections.Items[Connection.Index].MakeVisible(False);
    FDesigner.SelectComponent(FComponent[Connection.Index]);
    FDesigner.Modified;
end;

procedure TradConnectionEditor.cmDeleteUpdate(Sender: TObject);
begin
    cmDelete.Enabled := lvConnections.SelCount > 0;
end;

procedure TradConnectionEditor.cmMoveUpUpdate(Sender: TObject);
begin
    if lvConnections.SelCount = 1 then
        cmMoveUp.Enabled := lvConnections.Selected.Index > 0
    else
        cmMoveUp.Enabled := False;
end;

procedure TradConnectionEditor.cmMoveDownUpdate(Sender: TObject);
begin
    if lvConnections.SelCount = 1 then
        cmMoveDown.Enabled := lvConnections.Selected.Index < lvConnections.Items.Count - 1
    else
        cmMoveDown.Enabled := False;
end;

procedure TradConnectionEditor.FormActivate(Sender: TObject);
begin
    UpdateListBox;
end;

procedure TradConnectionEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if FComponent <> nil then
        FDesigner.SelectComponent(FComponent);

    with TRegIniFile.Create('Software\TrueRAD Soft\TrueRAD Suite\Connection Editor') do begin
        try
            WriteInteger('', 'Left', Left);
            WriteInteger('', 'Top', Top);
            WriteInteger('', 'Width', Width);
            WriteInteger('', 'Height', Height);
        finally
            Free;
        end;
    end;

    Action := caFree;
end;

procedure TradConnectionEditor.lvConnectionsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
    if Selected then
        FDesigner.SelectComponent(TradConnection(Item.Data));
end;

procedure TradConnectionEditor.FormCreate(Sender: TObject);
begin
    with TRegIniFile.Create('Software\TrueRAD Soft\TrueRAD Suite\Connection Editor') do begin
        try
            Left := ReadInteger('', 'Left', Left);
            Top := ReadInteger('', 'Top', Top);
            Width := ReadInteger('', 'Width', Width);
            Height := ReadInteger('', 'Height', Height);
        finally
            Free;
        end;
    end;

    ConnectionEditorsList.Add(Self);
end;

procedure TradConnectionEditor.FormDestroy(Sender: TObject);
begin
    if ConnectionEditorsList <> nil then
        ConnectionEditorsList.Remove(Self);
end;

initialization

finalization
    ConnectionEditorsList.Free;
    ConnectionEditorsList := nil;
end.
