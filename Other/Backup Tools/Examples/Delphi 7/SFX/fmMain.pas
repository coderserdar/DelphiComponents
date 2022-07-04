unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ArchiverRoot, CustExtractor, Extractor, FileCtrl,
  ComCtrls;

type
  TMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ListBox1: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    Extractor1: TExtractor;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure Extractor1Enumeration(Sender: TObject;
      const FileEntry: TFileEntry);
    procedure BitBtn1Click(Sender: TObject);
    procedure Extractor1StartOperation(Sender: TObject);
    procedure Extractor1FinishOperation(Sender: TObject);
    procedure Extractor1FileProgress(Sender: TObject; Percent: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Main: TMain;

implementation
{$R *.DFM}

procedure TMain.FormCreate(Sender: TObject);
begin
  // Use this line when debugging
  //Extractor1.FileName := ExtractFilePath(Application.ExeName)+'data.mmm';
  Extractor1.FileName := Application.ExeName;
  Label2.Caption := Extractor1.FileName;
  // Comment the following line when debugging
  Extractor1.SFXCodeSize := 392704;
  // Go
  Extractor1.Open;
end;

procedure TMain.Extractor1Enumeration(Sender: TObject;
  const FileEntry: TFileEntry);
begin
  with FileEntry do
    Listbox1.Items.Add( Name );
end;

procedure TMain.BitBtn1Click(Sender: TObject);
var
  dir : String;
begin
  dir := '';
  if SelectDirectory( dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0 ) then
    begin
      Extractor1.ExtractPath := dir;
      Extractor1.ExtractFiles;
      MessageDlg( 'Extraction done.', mtInformation, [mbOk], 0 );
    end;
end;

procedure TMain.Extractor1StartOperation(Sender: TObject);
begin
  ProgressBar1.Visible := True;
end;

procedure TMain.Extractor1FinishOperation(Sender: TObject);
begin
  ProgressBar1.Visible := False;
end;

procedure TMain.Extractor1FileProgress(Sender: TObject; Percent: Integer);
begin
  ProgressBar1.Position := Percent;
end;

end.
