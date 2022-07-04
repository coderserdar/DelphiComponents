unit DlgExport;

interface
         
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, checklst, Buttons, ComCtrls, Db, DUtils, Mask;

const
     cniSpaceDelimited = 0;
     cniTabDelimited   = 1;
     cniCommaDelimited = 2;
     cnsExportDir      = 'C:\Temp';
     cnsExportFile     = 'Export';


type
  TExportDialog = class(TForm)
    btnClose: TBitBtn;
    Bevel1: TBevel;
    cbxDriver: TComboBox;
    Label1: TLabel;
    btnUt: TSpeedButton;
    Label2: TLabel;
    dlgSave: TSaveDialog;
    aniMozg: TAnimate;
    btnExport: TBitBtn;
    btnMarkON: TSpeedButton;
    btnMarkOFF: TSpeedButton;
    clbFields: TCheckListBox;
    edOut: TEdit;
    pbWork: TProgressBar;
    procedure UtClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure MarkONClick(Sender: TObject);
    procedure MarkOFFClick(Sender: TObject);
    procedure DriverChange(Sender: TObject);
    procedure OutChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    { Private declarations }
    FExportDir : String;
    FDataSet   : TDataSet;
    FFields    : TStringList;

    // Load data to StringList
    procedure DataToStrings(List: TStrings; Delimiter: Char; FixLen: Boolean);

  public
    { Public declarations }

    property DataSet   : TDataSet read FDataSet   write FDataSet;
    property ExportDir : String   read FExportDir write FExportDir;

  end;

var
  ExportDialog: TExportDialog;

implementation


{$R *.DFM}

procedure TExportDialog.FormShow(Sender: TObject);
var
   i    : Integer;
begin
     FFields.Clear;
     clbFields.Items.Clear;
     for i := 0 to DataSet.Fields.Count-1 do
     begin
          FFields.Add(DataSet.Fields[i].FieldName);
          clbFields.Items.Add(Trim(DataSet.Fields[i].DisplayLabel));
          clbFields.Checked[i] := True;
     end;
     FExportDir := IIF(FExportDir = '', cnsExportDir, FExportDir);
     FExportDir := FExportDir + IIF(RightStr(FExportDir, 1) = '\', '', '\');
end;


// Output path settings
procedure TExportDialog.UtClick(Sender: TObject);
begin
     dlgSave.InitialDir := ExtractFileDir(edOut.Text);
     if dlgSave.InitialDir = '' then dlgSave.InitialDir := cnsExportDir;
     dlgSave.FileName   := ExtractFileName(edOut.Text);
     if dlgSave.FileName = '' then dlgSave.FileName := cnsExportFile;
     if dlgSave.Execute then
     begin
          edOut.Text := dlgSave.FileName;
          FExportDir := ExtractFileDir(dlgSave.FileName);
     end;
end;


// Start the Export
procedure TExportDialog.ExportClick(Sender: TObject);
var
   oTmp : TStringList;
   cDlm : Char;
begin
     btnExport.Enabled := False;
     aniMozg.Visible   := True;
     aniMozg.Active    := True;

     // Export data
     pbWork.Max := DataSet.RecordCount;
     if pbWork.Max <= 0 then pbWork.Max := 1000;
     pbWork.Position := 0;

     oTmp := TStringList.Create;
     try
     begin
     DataSet.DisableControls;
     try
     begin
          oTmp.Clear;
          cDlm := ' ';
          case cbxDriver.ItemIndex of
               cniSpaceDelimited : cDlm := ' ';
               cniTabDelimited   : cDlm := #9;
               cniCommaDelimited : cDlm := ',';
          end;
          DataToStrings(oTmp, cDlm, (cDlm = ' '));
          if oTmp.Count > 0 then oTmp.SaveToFile(edOut.Text);
          oTmp.Clear;
     end;
     finally
          DataSet.First;
          DataSet.EnableControls;
     end;
     end;
     finally
          oTmp.Free;
     end;

     pbWork.Position   := 0;
     aniMozg.Active    := False;
     aniMozg.Visible   := False;
     btnExport.Enabled := True;
     btnClose.SetFocus;
end;


// Load data to StringList
procedure TExportDialog.DataToStrings(List: TStrings; Delimiter: Char; FixLen: Boolean);
var
   i    : Integer;
   sTmp : String;
   oFld : TField;
begin
     List.Clear;
     DataSet.First;
     while not DataSet.Eof do
     begin
          sTmp := '';
          if FixLen then
          begin
               for i := 0 to clbFields.Items.Count-1 do
               begin
                    if clbFields.Checked[i] then
                    begin
                         oFld := DataSet.FieldByName(FFields.Strings[i]);
                         if Assigned(oFld) then
                         begin
                              sTmp := sTmp + PadR(oFld.AsString, oFld.DisplayWidth);
                         end;
                    end;
               end;
               List.Add(sTmp);
          end
          else
          begin
               for i := 0 to clbFields.Items.Count-1 do
               begin
                    if clbFields.Checked[i] then
                    begin
                         oFld := DataSet.FieldByName(FFields.Strings[i]);
                         if Assigned(oFld) then
                         begin
                              sTmp := sTmp + oFld.AsString + Delimiter;
                         end;
                    end;
               end;
               sTmp := LeftStr(sTmp, Length(sTmp)-1);
               List.Add(sTmp);
          end;
          pbWork.Position := pbWork.Position + 1;
          DataSet.Next;
     end;
end;


// All item select
procedure TExportDialog.MarkONClick(Sender: TObject);
var
   i : Integer;
begin
     for i := 0 to clbFields.Items.Count-1 do
     begin
          clbFields.Checked[i] := True;
     end;
end;


// Clear all selection
procedure TExportDialog.MarkOFFClick(Sender: TObject);
var
   i : Integer;
begin
     for i := 0 to clbFields.Items.Count-1 do
     begin
          clbFields.Checked[i] := False;
     end;
end;


// Set Driver
procedure TExportDialog.DriverChange(Sender: TObject);
var
   sExt : String;
begin
     if cbxDriver.ItemIndex > -1 then
     begin
          sExt := '.txt';
          case cbxDriver.ItemIndex of
               cniSpaceDelimited : sExt := '.prn';
               cniTabDelimited   : sExt := '.txt';
               cniCommaDelimited : sExt := '.csv';
          end;
          if edOut.Text <> '' then FExportDir := ExtractFilePath(edOut.Text);
          edOut.Enabled     := True;
          btnExport.Enabled := True;
          edOut.Text        := FExportDir + cnsExportFile + sExt;
          edOut.SetFocus;
     end;
end;


// Set path
procedure TExportDialog.OutChange(Sender: TObject);
begin
     btnExport.Enabled := ((cbxDriver.Text <> '') and (edOut.Text <> ''));
end;


procedure TExportDialog.FormCreate(Sender: TObject);
begin
     FFields := TStringList.Create;
end;


procedure TExportDialog.FormDestroy(Sender: TObject);
begin
     FFields.Free;
     FFields := nil;
end;


end.
