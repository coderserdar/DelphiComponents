unit Main;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ToolWin, ImgList, DBCtrls, DMaster, Db, DDB, DFilters, DbGrids, BlobF;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileOpenItem: TMenuItem;
    FileCloseItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    Edit1: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    StatusBar: TStatusBar;
    ActionList1: TActionList;
    FileExit1: TAction;
    FileOpen1: TAction;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowArrangeAll1: TWindowArrange;
    WindowMinimizeAll1: TWindowMinimizeAll;
    HelpAbout1: TAction;
    FileClose1: TWindowClose;
    WindowTileVertical1: TWindowTileVertical;
    WindowTileItem2: TMenuItem;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    tbCascade: TToolButton;
    tbHorizontal: TToolButton;
    tbVertical: TToolButton;
    ImageList1: TImageList;
    ToolButton12: TToolButton;
    FilterAdd: TAction;
    DataExport: TAction;
    FilterOne: TAction;
    FilterAll: TAction;
    FilterColor: TAction;
    ToolButton2: TToolButton;
    Master: TDMaster;
    Filters1: TMenuItem;
    miFilterAdd: TMenuItem;
    miFilterOne: TMenuItem;
    miFilterAll: TMenuItem;
    Export1: TMenuItem;
    ColorFilters1: TMenuItem;
    miMainColorFlt: TMenuItem;
    tbFilterAdd: TToolButton;
    tbFilterOne: TToolButton;
    tbFilterAll: TToolButton;
    ToolButton6: TToolButton;
    tbColorFlt: TToolButton;
    tbExport: TToolButton;
    tbFillUp: TToolButton;
    DataFillUp: TAction;
    ToolButton3: TToolButton;
    tbBlob: TToolButton;
    ShowBlob: TAction;
    procedure FileOpen1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure DataExportExecute(Sender: TObject);
    procedure FilterAddExecute(Sender: TObject);
    procedure FilterOneExecute(Sender: TObject);
    procedure FilterAllExecute(Sender: TObject);
    procedure FilterColorExecute(Sender: TObject);
    procedure DataFillUpExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure ShowBlobExecute(Sender: TObject);
  private
    { Private declarations }
    FBlobDlg : TBlobForm;

    procedure CreateMDIChild(const Name, TableName: string);
    procedure AddColorFilters();
    procedure ClickColor(Sender: TObject);
    procedure EnterColumn(Source: TDataSource; Field: TField);
    procedure OpenBlob(Source: TDataSource; Field: TField);
    procedure CloseBlob();

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses ChildWin, About;


procedure TMainForm.CreateMDIChild(const Name, TableName: string);
var
  Child: TMDIChild;
begin
     { create a new MDI child window }
     Child := TMDIChild.Create(Application);
     Child.Caption := Name;
     Child.Table.Connection   := Master.Connection;
     Child.OnColumn           := EnterColumn;
     Master.CentralData.GetADOTableNames(Child.cbxTable.Items);
     Child.cbxTable.ItemIndex := Child.cbxTable.Items.IndexOf(TableName);
     Child.Table.TableName    := Child.cbxTable.Text;
     if Child.Table.TableName <> '' then Child.Table.Open();
end;


procedure TMainForm.FileOpen1Execute(Sender: TObject);
begin
     if Master.Editor(1) <> '' then CreateMDIChild(Master.FileName, Master.CentralData.DefaultTable);
end;


procedure TMainForm.HelpAbout1Execute(Sender: TObject);
var
   AboutBox : TAboutBox;
begin
     AboutBox := TABoutBox.Create(Application);
     try
     begin
          if Assigned(ActiveMDIChild) then AboutBox.Version.Caption := 'ADO Version: ' + TMDIChild(ActiveMDIChild).Table.Version;
          AboutBox.ShowModal();
     end;
     finally
          AboutBox.Free();
     end;
end;


procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
     Close();
end;


procedure TMainForm.DataExportExecute(Sender: TObject);
begin
     if Assigned(ActiveMDIChild) then
     begin
          with TMDIChild(ActiveMDIChild) do
          begin
               Table.ExportEditor();
               Grid.Update();
          end;
     end;
end;


procedure TMainForm.FilterAddExecute(Sender: TObject);
begin
     if Assigned(ActiveMDIChild) then
     begin
          with TMDIChild(ActiveMDIChild) do
          begin
               if Table.FilterEditor(Grid.SelectedField) then
               begin
                    Grid.Update();
                    Table.ExecFilter();
                    FilterOne.Enabled := (Table.Filters.Count > 0);
                    FilterAll.Enabled := FilterOne.Enabled;
               end;
          end;
     end;
end;


procedure TMainForm.FilterOneExecute(Sender: TObject);
begin
     // Delete last item => default parameter is: Count-1
     if Assigned(ActiveMDIChild) then
     begin
          with TMDIChild(ActiveMDIChild) do
          begin
               if Table.Filters.Count > 0 then
               begin
                    Table.Filters.Delete();
                    Table.ExecFilter();
                    FilterOne.Enabled := (Table.Filters.Count > 0);
                    FilterAll.Enabled := FilterOne.Enabled;
               end;
          end;
     end;
end;

procedure TMainForm.FilterAllExecute(Sender: TObject);
begin
     // Delete all filter levels
     if Assigned(ActiveMDIChild) then
     begin
          with TMDIChild(ActiveMDIChild) do
          begin
               if Table.Filters.Count > 0 then
               begin
                    Table.Filters.Clear();
                    Table.ExecFilter();
                    FilterOne.Enabled := False;
                    FilterAll.Enabled := False;
               end;
          end;
     end;
end;


procedure TMainForm.FilterColorExecute(Sender: TObject);
begin
     // Add Color filters
     if Assigned(ActiveMDIChild) then
     begin
          with TMDIChild(ActiveMDIChild) do
          begin
               Table.ColorEditor();
               Grid.Update();
          end;
          AddColorFilters();
     end;
end;


procedure TMainForm.DataFillUpExecute(Sender: TObject);
begin
     if Assigned(ActiveMDIChild) then
     begin
          with TMDIChild(ActiveMDIChild) do
          begin
               Table.FillUpEditor(Grid.SelectedField);
               Grid.Update();
          end;
     end;
end;


procedure TMainForm.AddColorFilters();
var
   i      : Integer;
   miMain : TMenuItem;
   miChild: TMenuItem;
begin
     if Assigned(ActiveMDIChild) then
     begin
          with TMDIChild(ActiveMDIChild) do
          begin
               for i := miMainColorFlt.Count-1 downto 0 do
               begin
                    miMainColorFlt.Delete(i);
               end;
               for i := miColorFlt.Count-1 downto 0 do
               begin
                    miColorFlt.Delete(i);
               end;

               for i := 0 to Table.Colors.Count-1 do
               begin
                    miMain := TMenuItem.Create(miMainColorFlt);
                    miMain.Caption := Table.Colors.Captions[i];
                    miMain.OnClick := ClickColor;
                    miMainColorFlt.Add(miMain);

                    miChild := TMenuItem.Create(miColorFlt);
                    miChild.Caption := Table.Colors.Captions[i];
                    miChild.OnClick := ClickColor;
                    miColorFlt.Add(miChild);
               end;
          end;
     end;
end;


procedure TMainForm.ClickColor(Sender: TObject);
var
   oFilter : TDFilter;
   oColor  : TDFilter;
begin
     if Assigned(ActiveMDIChild) then
     begin
          with TMDIChild(ActiveMDIChild) do
          begin
               oColor := Table.Colors.FindItem((Sender as TMenuItem).Caption);
               if Assigned(oColor) then
               begin
                    oFilter := Table.Filters.Add();
                    oFilter.Assign(oColor);
                    Table.ReOpen();
                    FilterOne.Enabled := (Table.Filters.Count > 0);
                    FilterAll.Enabled := FilterOne.Enabled;
               end;
          end;
     end;
end;


procedure TMainForm.EnterColumn(Source: TDataSource; Field: TField);
begin
     CloseBlob();
     if tbBlob.Down  then OpenBlob(Source, Field);
end;


procedure TMainForm.OpenBlob(Source: TDataSource; Field: TField);
var
   oMemo : TDBMemo;
   oPict : TDBImage;
   oRich : TDBRichEdit;
begin
     if Assigned(Source) and Assigned(Field) then
     begin
          if Field is TBlobField then
          begin
               case TBlobField(Field).BlobType of
                    ftBlob        : ;                  // Binary Large Object field
                    ftMemo        :                    // Text memo field
                    begin
                         FBlobDlg := TBlobForm.Create(Application);
                         oMemo    := TDBMemo.Create(FBlobDlg);
                         oMemo.Parent     := FBlobDlg;
                         oMemo.Align      := alClient;
                         oMemo.DataSource := Source;
                         oMemo.DataField  := Field.FieldName;
                         FBlobDlg.Top     := Height - FBlobDlg.Height;
                         FBlobDlg.Left    := (Left + Width) - FBlobDlg.Width;
                         FBlobDlg.Show();
                    end;
                    ftGraphic     :                   // Bitmap field
                    begin
                         FBlobDlg := TBlobForm.Create(Application);
                         oPict    := TDBImage.Create(FBlobDlg);
                         oPict.Parent     := FBlobDlg;
                         oPict.Align      := alClient;
                         oPict.DataSource := Source;
                         oPict.DataField  := Field.FieldName;
                         oPict.Stretch    := True;
                         FBlobDlg.Top     := Height - FBlobDlg.Height;
                         FBlobDlg.Left    := (Left + Width) - FBlobDlg.Width;
                         FBlobDlg.Show();
                    end;
                    ftFmtMemo    :                    // Formatted text memo field
                    begin
                         FBlobDlg := TBlobForm.Create(Application);
                         oRich    := TDBRichEdit.Create(FBlobDlg);
                         oRich.Parent     := FBlobDlg;
                         oRich.Align      := alClient;
                         oRich.DataSource := Source;
                         oRich.DataField  := Field.FieldName;
                         FBlobDlg.Top     := Height - FBlobDlg.Height;
                         FBlobDlg.Left    := (Left + Width) - FBlobDlg.Width;
                         FBlobDlg.Show();
                    end;
                    ftParadoxOle : ;	              // Paradox OLE field
                    ftDBaseOle   : ;	              // dBase OLE field
                    ftTypedBinary: ;                  // Typed binary field
               end;
          end
          else CloseBlob();
     end
     else CloseBlob();
end;


procedure TMainForm.CloseBlob();
begin
     if Assigned(FBlobDlg) then
     begin
          FBlobDlg.Close();
          FBlobDlg := nil;
     end;
end;


procedure TMainForm.ShowBlobExecute(Sender: TObject);
begin
     if not tbBlob.Down and Assigned(FBlobDlg) then CloseBlob() else
     begin
          if Assigned(ActiveMDIChild) then
          begin
               with TMDIChild(ActiveMDIChild) do
               begin
                    OpenBlob(DataSource, Grid.SelectedField);
               end;
          end;
     end;
end;


procedure TMainForm.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
     FilterColor.Enabled := WindowCascade1.Enabled;
     FilterAdd.Enabled   := WindowCascade1.Enabled;
     DataExport.Enabled  := WindowCascade1.Enabled;
     DataFillUp.Enabled  := WindowCascade1.Enabled;
     Edit1.Enabled       := WindowCascade1.Enabled;
     tbBlob.Enabled      := WindowCascade1.Enabled;
end;


end.
