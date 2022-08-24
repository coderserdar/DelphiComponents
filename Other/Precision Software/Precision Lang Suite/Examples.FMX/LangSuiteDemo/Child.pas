unit Child;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs
  {$IF CompilerVersion >= 25.0 }
  , FMX.StdCtrls
  {$IFEND}
  ;

type
  TfrmChild = class(TForm)
    lbTest: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure LanguageChanged(Sender:TObject);
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
  end;

var
  frmChild: TfrmChild;

implementation

{$R *.fmx}

uses
  FMX.plsLangMan;

procedure TfrmChild.Assign(Source: TPersistent);
begin
  if Source is TLanguageManager then
    LanguageChanged(Source) // perform language changed event when language is changed by the user
  else
    inherited;
end;

procedure TfrmChild.FormCreate(Sender: TObject);
begin
  LanguageChanged(Self);  // perform language changed event for newly created form
end;

procedure TfrmChild.LanguageChanged(Sender:TObject);
begin
  if not LanguageManager.LangVCL(Self) then
    MessageDlg(LanguageManager.LanguageName+' ('+LanguageManager.LanguageCode+'): ['+Self.Name+' ('+Self.ClassName+')] '+LanguageManager.LastError,TMsgDlgType.mtWarning,[TMsgDlgBtn.mbOK],0);
end;

procedure TfrmChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

end.
