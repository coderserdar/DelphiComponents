unit mainf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, VideoCap, ExtCtrls,inifiles, StdCtrls, Clipbrd,
  ComCtrls, Buttons,vfw,mmsystem, Jpeg;

type
  TMain = class(TForm)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    CheckPrev: TCheckBox;
    GroupBox2: TGroupBox;
    BtSPict: TButton;
    EditSPict: TEdit;
    Panel2: TPanel;
    VideoCap1: TVideoCap;
    procedure VideoTreiber1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckPrevClick(Sender: TObject);
    procedure VideoFormat1Click(Sender: TObject);
    procedure VideoQuelle1Click(Sender: TObject);
    procedure BtSPictClick(Sender: TObject);
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  Main: TMain;

implementation

uses DlgTreiber, dlgaufz, aboutx;

{$R *.DFM}

procedure TMain.VideoTreiber1Click(Sender: TObject);
Var DrvList:TStrings;
begin
 DlgEinstell:=TDlgEinstell.Create(Self);
 drvList:=  GetDriverList;
 dlgEinstell.Combobox1.Items:= drvList;
 VideoCap1.DriverOpen:= false;
 dlgEinstell.ComboBox1.Itemindex:= VideoCap1.DriverIndex;
 if DlgEinstell.ShowModal = mrOK then
    begin
      videoCap1.DriverIndex:= dlgEinstell.combobox1.ItemIndex;
    end;

 VideoCap1.DriverOpen:= true;
 DlgEinstell.Free;
 drvList.Clear;
 drvList.Free;
end;

procedure TMain.FormCreate(Sender: TObject);
var ini:TInifile;

begin
  EditSPict.Text := ExtractFilePath(Application.ExeName) + 'Foto.jpg';
  ini:= TInifile.Create('vcap.ini');
  VideoCap1.Driverindex := 0;
  VideoCap1.Driverindex := ini.readInteger('Driver','index',0);
  ini.free;
end;

procedure TMain.FormDestroy(Sender: TObject);
var ini:TInifile;

begin
 ini:= TInifile.Create('vcap.ini');
 ini.writeInteger('Driver','index',videoCap1.Driverindex);
 ini.free;
end;

procedure TMain.FormShow(Sender: TObject);
begin
 VideoCap1.DriverOpen:=True;
 editsPict.text:= VideoCap1.SingleImageFile;
end;

procedure TMain.CheckPrevClick(Sender: TObject);
begin
 VideoCap1.VideoPreview:=CheckPrev.Checked;

end;

procedure TMain.VideoFormat1Click(Sender: TObject);
begin
 VideoCap1.DlgVFormat;
end;

procedure TMain.VideoQuelle1Click(Sender: TObject);
begin
 VideoCap1.DlgVSource;
end;

procedure TMain.BtSPictClick(Sender: TObject);
Var BMP : TBitMap;
    JPG : TJpegImage;
begin
  VideoCap1.SaveToClipboard;
  BMP := TBitMap.Create;
  BMP.LoadFromClipboardFormat(cf_BitMap,ClipBoard.GetAsHandle(cf_Bitmap),0);
  JPG := TJpegImage.Create;
  JPG.Assign(BMP);
  JPG.SaveToFile(EditSPict.Text);
  BMP.SaveToFile(EditSPict.Text+'.bmp');
  JPG.Free;
  BMP.Free;
end;

end.
