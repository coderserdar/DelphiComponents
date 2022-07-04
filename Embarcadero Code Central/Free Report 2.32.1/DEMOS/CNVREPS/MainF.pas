unit MainF;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl, ComCtrls, Buttons, FR_Desgn, FR_RRect,
  FR_Chart, FR_BarC, FR_Shape, FR_ChBox, FR_Rich, FR_Class;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    lbDirectory: TDirectoryListBox;
    Panel3: TPanel;
    cbDrive: TDriveComboBox;
    StatusBar: TStatusBar;
    lbFiles: TFileListBox;
    sbConvert: TSpeedButton;
    frReport1: TfrReport;
    sbPreview: TSpeedButton;
    frRichObject1: TfrRichObject;
    frCheckBoxObject1: TfrCheckBoxObject;
    frShapeObject1: TfrShapeObject;
    frBarCodeObject1: TfrBarCodeObject;
    frChartObject1: TfrChartObject;
    frRoundRectObject1: TfrRoundRectObject;
    frDesigner1: TfrDesigner;
    cbCreateBAK: TCheckBox;
    procedure cbDriveChange(Sender: TObject);
    procedure lbDirectoryChange(Sender: TObject);
    procedure lbFilesChange(Sender: TObject);
    procedure sbPreviewClick(Sender: TObject);
    procedure sbConvertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure FRELoadFromStream(Report: TfrReport; Stream: TStream);
var
  VersionB: Byte;
  Len1, Len2: Word;
  OldPosition: Longint;
begin
  try
    FRE_COMPATIBLE_READ := False;
    OldPosition := Stream.Position;
    Stream.ReadBuffer(VersionB, SizeOf(VersionB));
    if VersionB = 23 then
    begin
      Stream.Seek(4, soFromCurrent);
      Stream.ReadBuffer(Len1, SizeOf(Len1));
      Stream.ReadBuffer(Len2, SizeOf(Len2));
      if (Len1 >2) and (Len1 <= 255) and (Len2 = 0) then
        FRE_COMPATIBLE_READ := True;
    end;
    Stream.Position := OldPosition;
    Report.LoadFromStream(Stream);
  finally
    FRE_COMPATIBLE_READ := False;
  end;
end;

procedure FRELoadFromFile(Report: TfrReport; FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    FRELoadFromStream(Report, Stream);
    Report.FileName := FileName;
  finally
    Stream.Free;
  end;
end;

procedure TForm1.cbDriveChange(Sender: TObject);
begin
  lbDirectory.Drive := cbDrive.Drive;
end;

procedure TForm1.lbDirectoryChange(Sender: TObject);
begin
  lbFiles.Directory := lbDirectory.Directory;
end;

procedure TForm1.lbFilesChange(Sender: TObject);
var
  FileName, VersionStr: String;
  Stream: TFileStream;
  VersionB: Byte;
  Len1, Len2: Word;
  CnvEnabled: Boolean;
begin
  FileName := lbFiles.FileName;
  StatusBar.Panels[1].Text := FileName;
  VersionStr := '';
  CnvEnabled := False;
  if FileName <> '' then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      sbPreview.Enabled := True;
      Stream.ReadBuffer(VersionB, SizeOf(VersionB));
      VersionStr := 'Version: ' + IntToStr(VersionB);
      if VersionB = 23 then
      begin
        Stream.Seek(4, soFromCurrent);
        Stream.ReadBuffer(Len1, SizeOf(Len1));
        Stream.ReadBuffer(Len2, SizeOf(Len2));
        if (Hi(Len1) = 0) and (Len2 = 0) then
        begin
          VersionStr := 'Version: FRE >=2.21.7';
          CnvEnabled := True;
        end;
      end;
    finally
      Stream.Free;
    end;
  end
  else sbPreview.Enabled := False;

  sbConvert.Enabled := CnvEnabled;
  StatusBar.Panels[0].Text := VersionStr;
end;

procedure TForm1.sbPreviewClick(Sender: TObject);
var
  FileName: String;
begin
  FileName := lbFiles.FileName;
  if FileName <> '' then
  begin
    FRELoadFromFile(frReport1, Filename);
    frReport1.DesignReport;
  end;
end;

procedure TForm1.sbConvertClick(Sender: TObject);
var
  FileName, BakFileName: String;
begin
  FileName := lbFiles.FileName;
  if FileName <> '' then
  begin
    FRELoadFromFile(frReport1, Filename);
    if cbCreateBAK.Checked then
    begin
      BakFileName := ChangeFileExt(FileName, '.BAK');
      if FileExists(BakFileName) then
      begin
        ShowMessage('Convert failed: BAK file already exists!');
        Exit;
      end;
      RenameFile(FileName, BakFileName);
    end;
    frReport1.SaveToFile(Filename);
    lbFilesChange(lbFiles);
  end;
end;

end.
