unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ToolWin, SMSummInfo;

type
  TfrmMain = class(TForm)
    OpenDialog: TOpenDialog;
    tbMain: TToolBar;
    btnOpen: TToolButton;
    ImageList: TImageList;
    btnSeparator1: TToolButton;
    btnClose: TToolButton;
    btnAbout: TToolButton;
    btnSeparator2: TToolButton;
    lvSummary: TListView;
    SMSummaryInformation: TSMSummaryInformation;
    procedure btnOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    SMSummaryInformation.LoadFromFile(OpenDialog.FileName);

    lvSummary.Items.Clear;
    
    with lvSummary.Items.Add do
    begin
      Caption := 'FileName';
      SubItems.Add(OpenDialog.FileName)
    end;

    with lvSummary.Items.Add do
    begin
      Caption := 'Title';
      SubItems.Add(SMSummaryInformation.Title)
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'Subject';
      SubItems.Add(SMSummaryInformation.Subject)
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'Author';
      SubItems.Add(SMSummaryInformation.Author)
    end;

    with lvSummary.Items.Add do
    begin
      Caption := 'Keywords';
      SubItems.Add(SMSummaryInformation.Keywords)
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'Comments';
      SubItems.Add(SMSummaryInformation.Comments)
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'Template';
      SubItems.Add(SMSummaryInformation.Template)
    end;

    with lvSummary.Items.Add do
    begin
      Caption := 'LastAuthor';
      SubItems.Add(SMSummaryInformation.LastAuthor)
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'Created';
      SubItems.Add(DateToStr(SMSummaryInformation.Created))
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'LastSaved';
      SubItems.Add(DateToStr(SMSummaryInformation.LastSaved))
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'LastPrinted';
      SubItems.Add(DateToStr(SMSummaryInformation.LastPrinted))
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'RevNumber';
      SubItems.Add(SMSummaryInformation.RevNumber)
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'EditTime (min)';
      SubItems.Add(IntToStr(SMSummaryInformation.EditTime))
    end;

    with lvSummary.Items.Add do
    begin
      Caption := 'PageCount';
      SubItems.Add(IntToStr(SMSummaryInformation.PageCount))
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'CharCount';
      SubItems.Add(IntToStr(SMSummaryInformation.CharCount))
    end;
    with lvSummary.Items.Add do
    begin
      Caption := 'WordCount';
      SubItems.Add(IntToStr(SMSummaryInformation.WordCount))
    end;
  end;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  Application.MessageBox('TSMSummaryInformation component, written by Scalabium Software'#13#10 +
                         ''#13#10 +
                         'Allow to read summary information for any compound file/stream/storage'#13#10 +
                         'Direct and native document processing allow to extract an info very-very fast'#13#10 +
                         ''#13#10 +
                         'For additional tech information or support ask Mike Shkolnik:' + #13#10 +
                         'http://www.scalabium.com, e-mail: mshkolnik@scalabium.com' + #13#10,
                         'About SMComponent library', MB_OK);
end;


end.
