unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg, FCImage, ExtDlgs, ImgList, StdActns,
  ActnList, Menus, ComCtrls, ToolWin, ShellApi;

type
  TForm1 = class(TForm)
    FCImage1: TFCImage;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton11: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton12: TToolButton;
    ActionList1: TActionList;
    FileOpen1: TAction;
    ZoomIn: TAction;
    ZoomOut: TAction;
    BestFit: TAction;
    FileFirst: TAction;
    FilePrior: TAction;
    FileNext: TAction;
    FileLast: TAction;
    ImageList1: TImageList;
    tbResize: TToolButton;
    tbStretch: TToolButton;
    ToolButton17: TToolButton;
    Stretch: TAction;
    Resize: TAction;
    tbLock: TToolButton;
    LockZoom: TAction;
    StatusBar: TStatusBar;
    OpenPictureDialog: TOpenPictureDialog;
    Label1: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    procedure FCImage1ScaleChange(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure ComboBox1KeyPress(Sender: TObject; var Key: Char);
    procedure ZoomInExecute(Sender: TObject);
    procedure ZoomOutExecute(Sender: TObject);
    procedure BestFitExecute(Sender: TObject);
    procedure StretchExecute(Sender: TObject);
    procedure ResizeExecute(Sender: TObject);
    procedure LockZoomExecute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileFirstExecute(Sender: TObject);
    procedure FilePriorExecute(Sender: TObject);
    procedure FileNextExecute(Sender: TObject);
    procedure FileLastExecute(Sender: TObject);
    procedure FCImage1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FCImage1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Label1Click(Sender: TObject);
  private
    FFileIndex: Integer;
    procedure SetFileIndex( Value: Integer);
  public
    property FileIndex: Integer read FFileIndex write SetFileIndex;
    procedure LoadFile;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FCImage1ScaleChange(Sender: TObject);
begin
  ComboBox1.Text := FCImage1.ZoomString;
  StatusBar.Panels[0].Text:= FCImage1.ZoomString;
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
  FCImage1.ZoomString := ComboBox1.Text;
end;

procedure TForm1.ComboBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    FCImage1.SetFocus;
    Key := #0;
  end;
end;

procedure TForm1.ZoomInExecute(Sender: TObject);
begin
  FCImage1.StandardZoom(zaIn);
end;

procedure TForm1.ZoomOutExecute(Sender: TObject);
begin
  FCImage1.StandardZoom(zaOut);
end;

procedure TForm1.BestFitExecute(Sender: TObject);
begin
  FCImage1.StandardZoom(zaBestfit);
end;

procedure TForm1.StretchExecute(Sender: TObject);
begin
  if tbStretch.Down then
    FCImage1.ScaleMode := smStretch
  else
    FCImage1.ScaleMode := smScale;
end;

procedure TForm1.ResizeExecute(Sender: TObject);
begin
  if tbResize.Down then
    FCImage1.ScaleMode := smResize
  else
    FCImage1.ScaleMode := smScale;
end;

procedure TForm1.LockZoomExecute(Sender: TObject);
begin
  FCImage1.LockScale := tbLock.Down;
end;

procedure TForm1.FileOpen1Execute(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    FFileIndex := -1;
    FileIndex := 0;
  end;
end;

procedure TForm1.LoadFile;
begin
  if (FileIndex >= 0) and (FileIndex < OpenPictureDialog.Files.Count) then
    FCImage1.Picture.LoadFromFile(OpenPictureDialog.Files[FileIndex]);
end;

procedure TForm1.SetFileIndex(Value: Integer);
var S: string;
begin
  if Value <> FFileIndex then
  begin
    if (Value >= 0) and (Value < OpenPictureDialog.Files.Count) then
    begin
      FFileIndex := Value;
      S := OpenPictureDialog.Files[FFileIndex];
      FCImage1.Picture.LoadFromFile(S);
      StatusBar.Panels[1].Text := ExtractFileName(OpenPictureDialog.Files[FFileIndex]);
      FileNext.Enabled := True;
      FileLast.Enabled := True;
      FilePrior.Enabled := True;
      FileFirst.Enabled := True;
      if FFileIndex = OpenPictureDialog.Files.Count-1 then
      begin
        FileNext.Enabled := False;
        FileLast.Enabled := False;
      end;
      if FFileIndex = 0 then
      begin
        FilePrior.Enabled:= False;
        FileFirst.Enabled:= False;
      end;
    end;
  end;
end;

procedure TForm1.FileFirstExecute(Sender: TObject);
begin
  FileIndex := 0;
end;

procedure TForm1.FilePriorExecute(Sender: TObject);
begin
  FileIndex := FileIndex - 1;
end;

procedure TForm1.FileNextExecute(Sender: TObject);
begin
  FileIndex := FileIndex + 1;
end;

procedure TForm1.FileLastExecute(Sender: TObject);
begin
  FileIndex := OpenPictureDialog.Files.Count - 1;
end;

procedure TForm1.FCImage1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FileIndex := FileIndex + 1;
  Handled := True;
end;

procedure TForm1.FCImage1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  FileIndex := FileIndex - 1;
  Handled := True;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.farsicomponents.com/default.htm',
          nil, nil, SW_SHOWNORMAL)
end;

end.
