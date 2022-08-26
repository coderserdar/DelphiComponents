unit PathSel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, Buttons;

type
  ThhPathSelDialog = class (TComponent)
  private
    procedure SetPath( V : string ) ;
    procedure SetFilter( V : string ) ;
  protected
    fPath      : string ;
    fFilter    : string ;
    fCheckPath : boolean ;
    fTitle     : string ;
    fShowFiles : boolean ;
    fColumns   : integer ;
  public
    constructor Create(AOwner : TComponent) ; override ;
    destructor Destroy ; override ;
    function Execute : boolean ;
  published
    property CheckPath : boolean read fCheckPath write fCheckPath default true ;
    property Columns   : integer read fColumns write fColumns default 0 ;
    property Filter    : string read fFilter write SetFilter ;
    property Path      : string read fPath write SetPath ;
    property ShowFiles : boolean read fShowFiles write fShowFiles default true ;
    property Title     : string read fTitle write fTitle ;
  end ;

procedure Register;

implementation

type
  TPathSelForm = class(TForm)
    UpButton: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    DriveComboBox1: TDriveComboBox;
    OKbutton: TBitBtn;
    CancelButton: TBitBtn;
    Panel1: TPanel;
    Splitter1: TSplitter;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    Edit1: TEdit;
    procedure UpButtonClick(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure FilesLabel ;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

{$R *.DFM}

var
  PathSelForm : TPathSelForm ;

{ -------------------------------------------------------------------- }

constructor ThhPathSelDialog.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  fCheckPath := true ;
  fColumns   := 0 ;
  fFilter    := '*.*' ;
  fPath      := ExtractFilePath( Application.ExeName ) ;
  fShowFiles := true ;
  fTitle     := 'Select Path' ;
end ;

{ -------------------------------------------------------------------- }

destructor ThhPathSelDialog.Destroy;
begin
  inherited Destroy;
end ;

{ -------------------------------------------------------------------- }

procedure TPathSelForm.UpButtonClick(Sender: TObject);
var
  filepath : string ;

begin
  with DirectoryListBox1 do
  begin
    if ItemIndex > 0
    then begin
      filepath := directory ;
      if length(filepath) > 3
      then begin
        while filepath[length(filepath)] <> '\' do
        DELETE( filepath , length(filepath) , 1 ) ;
        directory := filepath ;
      end ;
    end ;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure TPathSelForm.DirectoryListBox1Change(Sender: TObject);
begin
  Edit1.Text := DirectoryListBox1.Directory ;
  UpButton.enabled := length(Edit1.Text) > 3 ;
end;

{ -------------------------------------------------------------------- }

procedure TPathSelForm.Splitter1Moved(Sender: TObject);
begin
  Label3.Left := FileListBox1.Left + 8 ;
end;

{ -------------------------------------------------------------------- }

procedure TPathSelForm.FilesLabel ;
begin
  Label3.Caption := 'Files ('+ FileListBox1.Mask + ')' ;
  Label3.Left := FileListBox1.Left + 8 ;
end;

{ -------------------------------------------------------------------- }

function ThhPathSelDialog.Execute : boolean ;
var
  CmdResult   : integer ;
  AllOK       : boolean ;

begin
  PathSelForm := TPathSelForm.Create( self ) ;
  with PathSelForm do
  begin
    if DirectoryExists( fPath )
    then DirectoryListBox1.Directory := fPath ;
    FileListBox1.Mask := fFilter ;
    Caption := fTitle ;
    Edit1.text := DirectoryListBox1.Directory ;
    DirectoryListBox1.Columns := fColumns ;
    UpButton.enabled := length(Edit1.Text) > 3 ;
    FilesLabel ;
    Label3.Visible := fShowFiles and (fFilter <> '') ;
    FileListBox1.Visible := Label3.Visible ;
    if not Label3.visible
      then DirectoryListBox1.Width := Panel1.ClientWidth-(Panel1.BevelWidth*2);
    repeat
      CmdResult := ShowModal ;
      AllOK := true ;
      if CmdResult = mrOK
      then begin
        if fCheckPath
        then begin
          if DirectoryExists( Edit1.Text )
          then begin
            fPath := Edit1.Text ;
            Execute := true ;
          end
          else begin
            ShowMessage( 'Path does not exist!' ) ;
            Execute := false ;
            AllOK := false ;
          end ;
        end
        else begin
          fPath := Edit1.Text ;
          Execute := true ;
        end ;
      end
      else begin
        Execute := false ;
      end ;
    until AllOK ;
    Destroy ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhPathSelDialog.SetPath( V : string ) ;
begin
  fPath := ExtractFilePath(V) ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhPathSelDialog.SetFilter( V : string ) ;
begin
  fFilter := V ;
end ;

{ ==================================================================== }

procedure Register;
begin
  RegisterComponents('Howie',[ThhPathSelDialog]) ;
end ;


end.
