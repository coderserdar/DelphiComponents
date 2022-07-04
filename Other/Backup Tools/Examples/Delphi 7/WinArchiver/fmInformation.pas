unit fmInformation;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ArchiverRoot, CustExtractor, CustArchiver, Archiver;

type
  TInformation = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lPath: TLabel;
    lName: TLabel;
    lSize: TLabel;
    lFiles: TLabel;
    lCompression: TLabel;
    lDate: TLabel;
    Label8: TLabel;
    lSegNum: TLabel;
    GroupBox1: TGroupBox;
    cbEncrypted: TCheckBox;
    cbCompressed: TCheckBox;
    cbSolid: TCheckBox;
    cbReadOnly: TCheckBox;
    cbFinalSegment: TCheckBox;
    procedure cbEncryptedClick(Sender: TObject);
    procedure cbCompressedClick(Sender: TObject);
    procedure cbSolidClick(Sender: TObject);
    procedure cbReadOnlyClick(Sender: TObject);
    procedure cbFinalSegmentClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Information: TInformation;

implementation
uses fmMain;
{$R *.DFM}

procedure TInformation.cbEncryptedClick(Sender: TObject);
begin
  cbEncrypted.Checked := afCrypted in Main.Archiver1.Header.ArchiveFlag;
end;

procedure TInformation.cbCompressedClick(Sender: TObject);
begin
  cbCompressed.Checked := afCompressed in Main.Archiver1.Header.ArchiveFlag;
end;

procedure TInformation.cbSolidClick(Sender: TObject);
begin
  cbSolid.Checked := (afSolid in Main.Archiver1.Header.ArchiveFlag) or Main.Archiver1.IsSolidArchive;
end;

procedure TInformation.cbReadOnlyClick(Sender: TObject);
begin
  cbReadOnly.Checked := afReadOnly in Main.Archiver1.Header.ArchiveFlag;
end;

procedure TInformation.cbFinalSegmentClick(Sender: TObject);
begin
  cbFinalSegment.Checked := afFinalSegment in Main.Archiver1.Header.ArchiveFlag;
end;

end.
