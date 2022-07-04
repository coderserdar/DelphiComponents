unit radpropindex;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, radcommon, radconlist, rade2m, DsgnIntf, ExtCtrls, StdCtrls;

type
  TradPropertyIndexEditor = class(TForm)
    sgIndexes: TStringGrid;
    Panel1: TPanel;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sgIndexesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure Panel1Resize(Sender: TObject);
  private
    FDesigner: IFormDesigner;
    FConnection: TObject;
    FPropName: String;
    FPropInfo: TradPropertyInfo;
    procedure UpdateListBox;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

procedure radShowPropertyIndexEditor(ADesigner: IDesigner; AConnection: TObject; APropName: String; APropInfo: TradPropertyInfo);

implementation

{$R *.DFM}

uses registry, typinfo;

var
  PropertyIndexEditorsList: TList = nil;

procedure radShowPropertyIndexEditor(ADesigner: IDesigner; AConnection: TObject; APropName: String; APropInfo: TradPropertyInfo);
var
    i: Integer;
    Editor: TradPropertyIndexEditor;
begin
    if PropertyIndexEditorsList = nil then
        PropertyIndexEditorsList := TList.Create;

    for i := 0 to PropertyIndexEditorsList.Count-1 do begin
        Editor := TradPropertyIndexEditor(PropertyIndexEditorsList[i]);
        with Editor do begin
            if (FDesigner = ADesigner) and (FConnection = AConnection) and
               (FPropName = APropName) then
            begin
                Show;
                BringToFront;
                Exit;
            end;
        end;
    end;

    Editor := TradPropertyIndexEditor.Create(Application);
    with Editor do
    try
        FDesigner := ADesigner as IFormDesigner;
        FConnection := AConnection;
        if FConnection is TComponent then
            TComponent(FConnection).FreeNotification(Editor)
        else if FConnection is TradParameterConnection then
            TradParameterConnection(FConnection).Connection.FreeNotification(Editor);
        FPropName := APropName;
        FPropInfo := APropInfo;
        ShowModal;
    except
        Free;
    end;
end;

procedure TradPropertyIndexEditor.UpdateListBox;
var
    i: Integer;
    Indexes: Variant;
begin
    Indexes := GetVariantProp(FConnection, FPropName);

    sgIndexes.RowCount := FPropInfo.IndexCount;
    for i := 0 to FPropInfo.IndexCount - 1 do begin
        sgIndexes.Cells[0, i] := FPropInfo.Indexes[i].Name;
        sgIndexes.Cells[1, i] := Indexes[i];
    end;
end;

procedure TradPropertyIndexEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if (AComponent = FConnection) or
           ((FConnection is TradParameterConnection) and
            (TradParameterConnection(FConnection).Connection = AComponent)) then
        begin
            FConnection := nil;
            Close;
        end;
    end;
end;

procedure TradPropertyIndexEditor.FormCreate(Sender: TObject);
begin
    with TRegIniFile.Create('Software\TrueRAD Soft\TrueRAD Suite\Property Index Editor') do begin
        try
            Left := ReadInteger('', 'Left', Left);
            Top := ReadInteger('', 'Top', Top);
            Width := ReadInteger('', 'Width', Width);
            Height := ReadInteger('', 'Height', Height);
        finally
            Free;
        end;
    end;

    PropertyIndexEditorsList.Add(Self);
end;

procedure TradPropertyIndexEditor.FormDestroy(Sender: TObject);
begin
    if PropertyIndexEditorsList <> nil then
        PropertyIndexEditorsList.Remove(Self);
end;

procedure TradPropertyIndexEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    {if FConnection <> nil then begin
        if FConnection is TComponent then
            FDesigner.SelectComponent(FConnection as TComponent)
        else if FConnection is TradParameterConnection then
            FDesigner.SelectComponent(TradParameterConnection(FConnection).Connection);
    end;}

    with TRegIniFile.Create('Software\TrueRAD Soft\TrueRAD Suite\Property Index Editor') do begin
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

procedure TradPropertyIndexEditor.FormActivate(Sender: TObject);
begin
    UpdateListBox;
end;

procedure TradPropertyIndexEditor.FormResize(Sender: TObject);
begin
    sgIndexes.ColWidths[1] := sgIndexes.ClientWidth;
end;

procedure TradPropertyIndexEditor.sgIndexesSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: String);
begin
    FPropInfo.Indexes[ARow].Value := Value;
end;

procedure TradPropertyIndexEditor.Panel1Resize(Sender: TObject);
begin
    btClose.Width := Panel1.ClientWidth;
end;

initialization

finalization
    PropertyIndexEditorsList.Free;
    PropertyIndexEditorsList := nil;
end.
