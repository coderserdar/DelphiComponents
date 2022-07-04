
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit radlinksedit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, rade2m, DsgnIntf;

type
  TradMethodParameterEditor = class(TForm)
    lvParameters: TListView;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvParametersSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDesigner: IFormDesigner;
    FConnection: TradE2MConnection;
    procedure UpdateListBox;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

procedure radShowMethodParameterEditor(ADesigner: IDesigner; AConnection: TradE2MConnection);

implementation

{$R *.DFM}

uses registry;

var
  MethodParameterEditorsList: TList = nil;

procedure radShowMethodParameterEditor(ADesigner: IDesigner; AConnection: TradE2MConnection);
var
    i: Integer;
    Editor: TradMethodParameterEditor;
begin
    if MethodParameterEditorsList = nil then
        MethodParameterEditorsList := TList.Create;

    for i := 0 to MethodParameterEditorsList.Count-1 do begin
        Editor := TradMethodParameterEditor(MethodParameterEditorsList[i]);
        with Editor do begin
            if (FDesigner = ADesigner) and (FConnection = AConnection) then begin
                Show;
                BringToFront;
                Exit;
            end;
        end;
    end;

    Editor := TradMethodParameterEditor.Create(Application);
    with Editor do
    try
        FDesigner := ADesigner as IFormDesigner;
        FConnection := AConnection;
        FConnection.FreeNotification(Editor);
        UpdateListbox;
        Show;
    except
        Free;
    end;
end;

procedure TradMethodParameterEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if (Operation = opRemove) and (AComponent = FConnection) then begin
        FConnection := nil;
        Close;
    end;
end;

procedure TradMethodParameterEditor.UpdateListBox;
var
    i: Integer;
    NewItem: TListItem;
begin
    lvParameters.Items.BeginUpdate;
    lvParameters.Items.Clear;
    try
        for i := 0 to FConnection.TargetMethodParameters.Count - 1 do begin
            NewItem := lvParameters.Items.Add;
            NewItem.Caption := FConnection.TargetMethodParameters[i].ParameterName;
            NewItem.Data := FConnection.TargetMethodParameters[i];
        end;
    finally
        lvParameters.Items.EndUpdate;
    end;
end;

procedure TradMethodParameterEditor.FormActivate(Sender: TObject);
begin
    UpdateListBox;
end;

procedure TradMethodParameterEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if FConnection <> nil then
        FDesigner.SelectComponent(FConnection);

    with TRegIniFile.Create('Software\TrueRAD Soft\TrueRAD Suite\Method Parameter Editor') do begin
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

procedure TradMethodParameterEditor.lvParametersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
    List: TDesignerSelectionList;
begin
    if Selected then begin
        List := TDesignerSelectionList.Create;
        try
            List.Add(TradParameterConnection(Item.Data));
            FDesigner.SetSelections(List);
        finally
            List.Free;
        end;
    end;
end;

procedure TradMethodParameterEditor.FormDestroy(Sender: TObject);
begin
    if MethodParameterEditorsList <> nil then
        MethodParameterEditorsList.Remove(Self);
end;

procedure TradMethodParameterEditor.FormCreate(Sender: TObject);
begin
    with TRegIniFile.Create('Software\TrueRAD Soft\TrueRAD Suite\Method Parameter Editor') do begin
        try
            Left := ReadInteger('', 'Left', Left);
            Top := ReadInteger('', 'Top', Top);
            Width := ReadInteger('', 'Width', Width);
            Height := ReadInteger('', 'Height', Height);
        finally
            Free;
        end;
    end;

    MethodParameterEditorsList.Add(Self);
end;

initialization

finalization
    MethodParameterEditorsList.Free;
    MethodParameterEditorsList := nil;
end.
