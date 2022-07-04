unit Replace;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, KEditCommon, Search;

type

  { TReplaceForm }

  TReplaceForm = class(TSearchForm)
    LBReplaceText: TLabel;
    CBTextToReplace: TComboBox;
    BUReplaceAll: TButton;
    CBPromptOnReplace: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure CBTextToReplaceClick(Sender: TObject);
    procedure CBTextToFindChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GetData(var Data: TKEditSearchData); override;
    procedure SetData(const Data: TKEditSearchData; SelAvail: Boolean); override;
  end;

var
  ReplaceForm: TReplaceForm;

implementation


procedure TReplaceForm.FormShow(Sender: TObject);
begin
  CBTextToFindChange(Sender);
end;

procedure TReplaceForm.CBTextToFindChange(Sender: TObject);
begin
  BUFind.Enabled := (CBTextToFind.Text <> '');
  BUReplaceAll.Enabled := BUFind.Enabled;
end;

procedure TReplaceForm.CBTextToReplaceClick(Sender: TObject);
begin
  if (CBTextToReplace.Text <> '') and
    (CBTextToReplace.Items.IndexOf(CBTextToReplace.Text) < 0) then
    CBTextToReplace.Items.Insert(0, CBTextToReplace.Text);
end;

procedure TReplaceForm.GetData(var Data: TKEditSearchData);
begin
  inherited;
  with Data do
  begin
    if CBPromptOnReplace.Checked then Include(Options, esoPrompt);
    TextToReplace := CBTextToReplace.Text;
  end;
end;

procedure TReplaceForm.SetData(const Data: TKEditSearchData; SelAvail: Boolean);
begin
  inherited;
  with Data do
    CBPromptOnReplace.Checked := esoPrompt in Options;
end;

{$IFDEF FPC}
initialization
  {$i replace.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
