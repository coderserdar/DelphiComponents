{*******************************************************}
{                                                       }
{       Report Designer                                 }
{       Extension Library example of                    }
{       TELDesigner, TELDesignPanel                     }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit frmDocUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables, ELDsgnr, ExtCtrls, QuickRpt, QRCtrls, ELControls, Menus,
  ELUtils, StdCtrls;

type
  TNotifyQuickRep = class(TQuickRep)
  private
    FOnNotification: TELDesignerOnNotificationEvent;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    property OnNotification: TELDesignerOnNotificationEvent read FOnNotification write FOnNotification;
  end;

  TfrmDoc = class(TForm)
    ELDesignPanel1: TELDesignPanel;
    ELDesigner1: TELDesigner;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure ELDesigner1ControlInserting(Sender: TObject;
      var AControlClass: TControlClass);
    procedure ELDesigner1ControlInserted(Sender: TObject);
    procedure ELDesigner1KeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
    procedure ELDesigner1ChangeSelection(Sender: TObject);
    procedure ELDesigner1Modified(Sender: TObject);
    procedure ELDesigner1DblClick(Sender: TObject);
    procedure ELDesigner1ControlHint(Sender: TObject; AControl: TControl;
      var AHint: String);
    procedure ELDesigner1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FDataSet: TBDEDataSet;
    FReport: TNotifyQuickRep;
    FFileName: string;
    FModified: Boolean;
    procedure SetDataSet(const Value: TBDEDataSet);
    procedure AdjustChangeSelection;
    procedure ReportNotification(Sender: TObject; AnObject: TPersistent;
      Operation: TOperation);
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    constructor CreateDocument(AOwner: TComponent; AFileName: string);
    procedure Save;
    procedure SaveAs(AFileName: string);
    procedure Modify;
    property DataSet: TBDEDataSet read FDataSet write SetDataSet;
    property Report: TNotifyQuickRep read FReport;
    property Designer: TELDesigner read ELDesigner1;
    property FileName: string read FFileName;
    property Modified: Boolean read FModified;
  end;

var
  frmDoc: TfrmDoc;
  FormWasClosed: Boolean;

implementation

uses frmMainUnit, frmPropsUnit, dlgFieldsUnit, dlgLinesEditorUnit;

{$R *.dfm}

{ TForm3 }

var
  DocNum: Integer = 1;
  Signature: packed array[0..3] of Char = ('E', 'L', 'R', 'P');

procedure TfrmDoc.SetDataSet(const Value: TBDEDataSet);
var
  LI: Integer;
begin
  if FDataSet <> nil then FDataSet.Free;
  FDataSet := Value;
  FReport.DataSet := Value;
  for LI := 0 to FReport.ComponentCount - 1 do
    if FReport.Components[LI] is TQRDBText then
      TQRDBText(FReport.Components[LI]).DataSet := FDataSet;
end;

procedure TfrmDoc.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if Modified then
    case MessageDlg('Save document "' + Caption + '"?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        if FileName <> '' then
          frmMain.Save(Self)
        else
          if not frmMain.SaveAs(Self) then Action := caNone;
      mrNo: {Do nothind};
      mrCancel: Action := caNone;
    end;
  FormWasClosed := Action = caFree;
  if Action = caFree then
    frmMain.UpdateZoomComboBox(True);
end;

procedure TfrmDoc.FormDestroy(Sender: TObject);
begin
  ELDesigner1.SelectedControls.Clear;
  ELDesigner1.Active := False;
  FReport.Free;
  if frmProps.Doc = Self then frmProps.Doc := nil;
end;

procedure TfrmDoc.ELDesigner1ControlInserting(Sender: TObject;
  var AControlClass: TControlClass);
begin
  frmMain.ControlInserting(AControlClass);
end;

procedure TfrmDoc.ELDesigner1ControlInserted(Sender: TObject);
begin
  frmMain.ControlInserted;
end;

procedure TfrmDoc.ELDesigner1KeyPress(Sender: TObject; var Key: Char);

  function _SetString(var AStr: string): Boolean;
  begin
    Result := False;
    if Ord(Key) >= 32 then
    begin
      AStr := AStr + Key;
      Result := True;
    end
    else if Ord(Key) = VK_BACK then
    begin
      AStr := Copy(AStr, 1, Length(AStr) - 1);
      Result := True;
    end;
  end;

var
  LC: TControl;
  LS: string;

begin
  if ELDesigner1.SelectedControls.Count = 1 then
    LC := ELDesigner1.SelectedControls.DefaultControl
  else
    LC := nil;
  if LC <> nil then
  begin
    if LC.ClassType = TQRLabel then
    begin
      LS := TQRLabel(LC).Caption;
      if _SetString(LS) then
      begin
        TQRLabel(LC).Caption := LS;
        ELDesigner1.Modified;
      end;
    end else if LC.ClassType = TQRExpr then
    begin
      if Ord(Key) = VK_RETURN then
      begin
        LS := TQRExpr(LC).Expression;
        if dlgLinesEditor.Execute(LS, DataSet) then
        begin
          TQRExpr(LC).Expression := LS;
          ELDesigner1.Modified;
        end;
      end
      else
      begin
        LS := TQRExpr(LC).Expression;
        if _SetString(LS) then
        begin
          TQRExpr(LC).Expression := LS;
          ELDesigner1.Modified;
        end;
      end;
    end else if LC.ClassType = TQRDBText then
    begin
      if Ord(Key) = VK_RETURN then
      begin
        LS := TQRDBText(LC).DataField;
        if dlgFields.Execute(DataSet, LS) then
        begin
          TQRDBText(LC).DataField := LS;
          ELDesigner1.Modified;
        end;
      end
      else
      begin
        LS := TQRDBText(LC).DataField;
        if _SetString(LS) then
        begin
          TQRDBText(LC).DataField := LS;
          ELDesigner1.Modified;
        end;
      end;
    end else if (LC.ClassType = TQRMemo) and (Ord(Key) = VK_RETURN) then
    begin
      if dlgLinesEditor.Execute(TQRMemo(LC).Lines, DataSet) then
      begin
        TQRMemo(LC).Refresh;
        ELDesigner1.Modified;
      end;
    end else if (LC.ClassType = TQRExprMemo) and (Ord(Key) = VK_RETURN) then
    begin
      if dlgLinesEditor.Execute(TQRExprMemo(LC).Lines, DataSet) then
      begin
        TQRExprMemo(LC).Refresh;
        ELDesigner1.Modified;
      end;
    end;
  end;
end;

procedure TfrmDoc.FormActivate(Sender: TObject);
begin
  frmProps.Doc := Self;
  AdjustChangeSelection;
  frmMain.UpdateZoomComboBox(False);
end;

procedure TfrmDoc.AdjustChangeSelection;
var
  LObjects: TList;
begin
  if frmProps.Doc = Self then
  begin
    LObjects := TList.Create;
    try
	  ELDesigner1.SelectedControls.GetControls(LObjects);
      frmProps.PropInsp.AssignObjects(LObjects);
    finally
      LObjects.Free;
    end;
  end;
end;

procedure TfrmDoc.ELDesigner1ChangeSelection(Sender: TObject);
begin
  AdjustChangeSelection;
end;

procedure TfrmDoc.ELDesigner1Modified(Sender: TObject);
begin
  if frmProps.Doc = Self then frmProps.PropInsp.Modified;
  Modify;
end;

{ TNotifyQuickRep }

procedure TNotifyQuickRep.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Assigned(OnNotification) then OnNotification(Self, AComponent, Operation);
end;

procedure TfrmDoc.ReportNotification(Sender: TObject;
  AnObject: TPersistent; Operation: TOperation);
begin
  if Operation = opInsert then
  begin
    if AnObject is TQRDBText then TQRDBText(AnObject).DataSet := FDataSet;
  end;
end;

procedure TfrmDoc.ELDesigner1DblClick(Sender: TObject);
var
  LS: string;
  LControl: TControl;
begin
  if ELDesigner1.SelectedControls.Count = 1 then
  begin
    LControl := ELDesigner1.SelectedControls.DefaultControl;
    if LControl is TQRDBText then
    begin
      if dlgFields.Execute(DataSet, LS) then
      begin
        TQRDBText(LControl).DataField := LS;
        ELDesigner1.Modified;
      end;
    end else if LControl is TQRMemo then
    begin
      if dlgLinesEditor.Execute(TQRMemo(LControl).Lines, DataSet) then
      begin
        TQRMemo(LControl).Refresh;
        ELDesigner1.Modified;
      end;
    end else if LControl is TQRExprMemo then
    begin
      if dlgLinesEditor.Execute(TQRExprMemo(LControl).Lines, DataSet) then
      begin
        TQRExprMemo(LControl).Refresh;
        ELDesigner1.Modified;
      end;
    end else if LControl is TQRExpr then
    begin
      LS := TQRExpr(LControl).Expression;
      if dlgLinesEditor.Execute(LS, DataSet) then
      begin
        TQRExpr(LControl).Expression := LS;
        ELDesigner1.Modified;
      end;
    end;
  end;
end;

procedure TfrmDoc.ELDesigner1ControlHint(Sender: TObject;
  AControl: TControl; var AHint: String);
var
  LI: Integer;
begin
  if AControl is TQRMemo then
  begin
    AHint := AHint + #13 + 'Lines:';
    for LI := 0 to TQRMemo(AControl).Lines.Count - 1 do
      AHint := AHint + #13 + '  ' + TQRMemo(AControl).Lines[LI];
  end else if AControl is TQRExprMemo then
  begin
    AHint := AHint + #13 + 'Lines:';
    for LI := 0 to TQRExprMemo(AControl).Lines.Count - 1 do
      AHint := AHint + #13 + '  ' + TQRExprMemo(AControl).Lines[LI];
  end else if AControl is TQRLabel then
  begin
    AHint := AHint + #13 + 'Caption: ' + TQRLabel(AControl).Caption;
  end else if AControl is TQRDBText then
  begin
    AHint := AHint + #13 + 'Data field: ' + TQRDBText(AControl).DataField;
  end else if AControl is TQRExpr then
  begin
    AHint := AHint + #13 + 'Expression: ' + TQRExpr(AControl).Expression;
  end;  
end;

procedure TfrmDoc.ELDesigner1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LC: TControl;
begin
  if (ELDesigner1.SelectedControls.Count = 1) and (Key = VK_DELETE) then
  begin
    LC := ELDesigner1.SelectedControls.DefaultControl;
    if LC.ClassType = TQRLabel then
    begin
      if TQRLabel(LC).Caption <> '' then
      begin
        TQRLabel(LC).Caption := '';
        Key := 0;
        ELDesigner1.Modified;
      end;
    end else if LC.ClassType = TQRExpr then
    begin
      if TQRExpr(LC).Expression <> '' then
      begin
        TQRExpr(LC).Expression := '';
        Key := 0;
        ELDesigner1.Modified;
      end;
    end else if LC.ClassType = TQRDBText then
    begin
      if TQRDBText(LC).DataField <> '' then
      begin
        TQRDBText(LC).DataField := '';
        Key := 0;
        ELDesigner1.Modified;
      end;
    end
  end;
end;

constructor TfrmDoc.CreateDocument(AOwner: TComponent; AFileName: string);
begin
  Create(AOwner);
  LoadFromFile(AFileNAme);
  FFileName := AFileNAme;
  Caption := ExtractFileName(AFileName);
  Dec(DocNum);
end;

procedure TfrmDoc.Save;
begin
  SaveToFile(FFileName);
  FModified := False;
end;

procedure TfrmDoc.SaveAs(AFileName: string);
begin
  SaveToFile(AFileName);
  FFileName := AFileName;
  Caption := ExtractFileName(AFileName);
  FModified := False;
end;

procedure TfrmDoc.LoadFromFile(AFileName: string);
var
  LS: TFileStream;
  LI: Longint;
  LB: Byte;
  LDataSet: TBDEDataSet;
begin
  LS := TFileStream.Create(AFileName, fmOpenRead);
  try
    LS.Read(LI, SizeOf(Longint));
    if LI <> Longint(Signature) then
      raise Exception.Create('File "' + ExtractFileName(AFileName) +
        '" is not a report file');

    LS.Read(LB, SizeOf(Byte));
    case LB of
      1:
        begin
          LDataSet := TQuery.Create(Self);
          try
            TQuery(LDataSet).DatabaseName := ELLoadStringFromStream(LS);
            ELUnpackStrings(ELLoadStringFromStream(LS), TQuery(LDataSet).SQL);
            LDataSet.Open;
          except
            LDataSet.Free;
            raise;
          end;
          DataSet := LDataSet;
        end;
      2:
        begin
          LDataSet := TTable.Create(Self);
          try
            TTable(LDataSet).DatabaseName := ELLoadStringFromStream(LS);
            TTable(LDataSet).TableName := ELLoadStringFromStream(LS);
            LDataSet.Open;
          except
            LDataSet.Free;
            raise;
          end;
          DataSet := LDataSet;
        end;
    end;
    LS.ReadComponent(FReport);
    FReport.Left := 0;
    FReport.Top := 0;
  finally
    LS.Free;
  end;
end;

procedure TfrmDoc.SaveToFile(AFileName: string);
var
  LS: TFileStream;
  LI: Longint;
  LB: Byte;
begin
  LS := TFileStream.Create(AFileName, fmCreate);
  try
    LI := Longint(Signature);
    LS.Write(LI, SizeOf(Longint));
    if FDataSet <> nil then
    begin
      if FDataSet is TQuery then LB := 1
        else if FDataSet is TTable then LB := 2;
    end else LB := 0;
    LS.Write(LB, SizeOf(Byte));
    case LB of
      1:
        begin
          ELSaveStringToStream(TQuery(DataSet).DatabaseName, LS);
          ELSaveStringToStream(ELPackStrings(TQuery(DataSet).SQL), LS);
        end;
      2:
        begin
          ELSaveStringToStream(TTable(DataSet).DatabaseName, LS);
          ELSaveStringToStream(TTable(DataSet).TableName, LS);
        end;
    end;
    LS.WriteComponent(FReport);
  finally
    LS.Free;
  end;
end;

constructor TfrmDoc.Create(AOwner: TComponent);
begin
  inherited;
  FReport := TNotifyQuickRep.Create(nil);
  FReport.Name := 'Report';
  FReport.OnNotification := ReportNotification;
  FReport.PrintIfEmpty := True;
  ELDesigner1.DesignControl := FReport;
  ELDesigner1.Active := True;

  Caption := 'Document' + IntToStr(DocNum);
  Inc(DocNum);
end;

procedure TfrmDoc.Modify;
begin
  FModified := True;
end;

end.
