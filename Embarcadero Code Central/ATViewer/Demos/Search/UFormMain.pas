{$I ATStreamSearchOptions.inc}

unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  TntStdCtrls, TntDialogs,
  ATBinHex, ATStreamSearch;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Search: TATStreamSearch;
    OpenDialog1: TTntOpenDialog;
    GroupBox1: TGroupBox;
    labFN: TLabel;
    edFilename: TTntEdit;
    btnBrowse: TButton;
    chkOEM: TCheckBox;
    chkUnicode: TCheckBox;
    chkUnicodeBE: TCheckBox;
    GroupBox2: TGroupBox;
    labString: TLabel;
    edString: TTntEdit;
    btnFind: TButton;
    btnFindNext: TButton;
    labOptions: TLabel;
    chkCase: TCheckBox;
    chkWords: TCheckBox;
    chkRegex: TCheckBox;
    chkBack: TCheckBox;
    Label1: TLabel;
    GroupBoxViewer: TGroupBox;
    Viewer: TATBinHex;
    Panel2: TPanel;
    Label2: TLabel;
    edSelection: TTntEdit;
    Label3: TLabel;
    edOffset: TTntEdit;
    Label4: TLabel;
    edLength: TTntEdit;
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure edStringChange(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure chkUnicodeClick(Sender: TObject);
    procedure SearchProgress(const ACurrentPos,
      AMaximalPos: Int64; var AContinueSearching: Boolean);
    procedure chkOEMClick(Sender: TObject);
    procedure chkRegexClick(Sender: TObject);

  private
    { Private declarations }
    FFileName: WideString;

  public
    { Public declarations }
    procedure Find(AFindFirst: Boolean);
  end;

var
  FormMain: TFormMain;

implementation

uses
  ATxCodepages,
  UFormProgress;

{$R *.DFM}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FFileName := '';

  {$IFNDEF REGEX}
  chkRegex.Checked := False;
  chkRegex.Enabled := False;
  chkRegex.Caption := chkRegex.Caption + ' (RegEx library not compiled in)';
  {$ENDIF}
end;

procedure TFormMain.btnBrowseClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      begin
        FFileName := FileName;
        edFilename.Text := FFileName;
        edStringChange(Self);
        if Viewer.Open(FFileName) then
          begin
            chkUnicodeBE.Checked :=
              Viewer.FileUnicodeFormat = vbUnicodeFmtBE;
          end
        else
          begin
            WideMessageDlg('Could not open "' + FFileName + '"', mtError, [mbOK], 0);
            { Application.MessageBox(
                PChar('Could not open "' + FFileName + '"'),
                'Error', MB_OK or MB_ICONERROR); }
          end;
      end;
end;

procedure TFormMain.edStringChange(Sender: TObject);
begin
  btnFind.Enabled := (edString.Text <> '') and (edFilename.Text <> '');
  btnFindNext.Enabled := btnFind.Enabled and (Search.FoundStart >= 0);
end;

procedure TFormMain.Find(AFindFirst: Boolean);
var
  OK: Boolean;
  Encoding: TATEncoding;
  Options: TATStreamSearchOptions;
begin
  if AFindFirst then
    begin
      Viewer.SetSelection(0, 0, True);
      edSelection.Text := '';
      edOffset.Text := '';
      edLength.Text := '';
    end;

  OK := False;

  try
    try
      Self.Enabled := False;
      FormProgress.Show;

      if chkUnicode.Checked then
        begin
          if chkUnicodeBE.Checked then
            Encoding := vencUnicodeBE
          else
            Encoding := vencUnicodeLE;
        end
      else
        begin
          if chkOEM.Checked then
            Encoding := vencOEM
          else
            Encoding := vencANSI;
        end;

      Options := [];

      {$IFDEF REGEX}
      if chkRegex.Checked then
        Include(Options, asoRegEx);
      {$ENDIF}

      if chkCase.Checked then
        Include(Options, asoCaseSens);

      if chkWords.Checked then
        Include(Options, asoWholeWords);

      if chkBack.Checked then
        Include(Options, asoBackward);


      if AFindFirst then
      begin
        try
          Search.FileName := FFileName;
        except
          Application.MessageBox('Cannot open filename specified', 'Error', MB_OK or MB_ICONERROR);
          Exit;
        end;
        OK := Search.FindFirst(edString.Text, 0, Encoding, Options);
      end
      else
      begin
        OK := Search.FindNext;
      end;

    finally
      FormProgress.Hide;
      Self.Enabled := True;
    end;

  except
    on E: Exception do
      if E.Message <> '' then
      begin
        Application.MessageBox(PChar(E.Message), 'Search failed', MB_OK or MB_ICONERROR);
        Exit;
      end; 
  end;

  if OK then
    begin
      Viewer.SetSelection(Search.FoundStart, Search.FoundLength, True);

      if Viewer.Mode = vbmodeUnicode then
        edSelection.Text := Viewer.SelTextW
      else
        edSelection.Text := Viewer.SelText;

      edOffset.Text := IntToStr(Search.FoundStart);
      edLength.Text := IntToStr(Search.FoundLength);
    end
  else
    Application.MessageBox('Search string not found', 'Search failed', MB_OK or MB_ICONERROR);

  edStringChange(Self);
end;

procedure TFormMain.btnFindClick(Sender: TObject);
begin
  Find(True);
end;

procedure TFormMain.btnFindNextClick(Sender: TObject);
begin
  Find(False);
end;

procedure TFormMain.chkUnicodeClick(Sender: TObject);
begin
  if chkUnicode.Checked then
    Viewer.Mode := vbmodeUnicode
  else
    Viewer.Mode := vbmodeText;

  chkUnicodeBE.Checked :=
    Viewer.FileUnicodeFormat = vbUnicodeFmtBE;

  chkOEM.Enabled := not chkUnicode.Checked;

  edStringChange(Self);
end;

procedure TFormMain.chkOEMClick(Sender: TObject);
begin
  if chkOEM.Checked then
    Viewer.TextEncoding := vencOEM
  else
    Viewer.TextEncoding := vencANSI;

  edStringChange(Self);
end;

procedure TFormMain.SearchProgress(const ACurrentPos,
  AMaximalPos: Int64; var AContinueSearching: Boolean);
begin
  AContinueSearching := FormProgress.Progress(ACurrentPos, AMaximalPos);
end;

procedure TFormMain.chkRegexClick(Sender: TObject);
begin
  chkBack.Enabled := not chkRegex.Checked;
end;

end.

