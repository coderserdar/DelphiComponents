unit Search;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, KEditCommon;

type
  TSearchForm = class(TForm)
    CBTextToFind: TComboBox;
    GBOptions: TGroupBox;
    LBFindText: TLabel;
    BUFind: TButton;
    BUCancel: TButton;
    CBMatchCase: TCheckBox;
    CBHexaSearch: TCheckBox;
    GBDirection: TGroupBox;
    RBForward: TRadioButton;
    RBBackward: TRadioButton;
    GBScope: TGroupBox;
    RBGlobal: TRadioButton;
    RBSelectedOnly: TRadioButton;
    GBOrigin: TGroupBox;
    RBFromCursor: TRadioButton;
    RBEntireScope: TRadioButton;
    procedure BUFindClick(Sender: TObject);
    procedure CBTextToFindChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GetData(var Data: TKEditSearchData); virtual;
    procedure SetData(const Data: TKEditSearchData; SelAvail: Boolean); virtual;
  end;

var
  SearchForm: TSearchForm;

function TrimToSize(const Text: string; Size: Integer): string;

implementation

uses Options;

function TrimToSize(const Text: string; Size: Integer): string;
begin
  Result := Text;
  if (Size > 0) and (Length(Result) > Size) then
  begin
    SetLength(Result, Size);
    Result := Format('%s...', [Result]);
  end;
end;

procedure TSearchForm.BUFindClick(Sender: TObject);
begin
  if CBTextToFind.Items.IndexOf(CBTextToFind.Text) < 0 then
    CBTextToFind.Items.Insert(0, CBTextToFind.Text);
end;

procedure TSearchForm.CBTextToFindChange(Sender: TObject);
begin
  BUFind.Enabled := CBTextToFind.Text <> '';
end;

procedure TSearchForm.FormShow(Sender: TObject);
begin
  CBTextToFindChange(Sender);
end;

procedure TSearchForm.GetData(var Data: TKEditSearchData);
begin
  with Data do
  begin
    Options := [];
    if CBMatchCase.Checked then Include(Options, esoMatchCase);
    if CBHexaSearch.Checked then Include(Options, esoTreatAsDigits);
    if RBBackward.Checked then Include(Options, esoBackwards);
    if RBEntireScope.Checked then Include(Options, esoEntireScope);
    if RBSelectedOnly.Checked then Include(Options, esoSelectedOnly);
    TextToFind := CBTextToFind.Text;
  end;
end;

procedure TSearchForm.SetData(const Data: TKEditSearchData; SelAvail: Boolean);
begin
  ActiveControl := CBTextToFind;
  with Data do
  begin
    CBMatchCase.Checked := esoMatchCase in Options;
    CBHexaSearch.Checked := esoTreatAsDigits in Options;
    if esoBackwards in Options then
      RBBackward.Checked := True
    else
      RBForward.Checked := True;
    if esoEntireScope in Options then
      RBEntireScope.Checked := True
    else
      RBFromCursor.Checked := True;
    if SelAvail then
    begin
      RBSelectedOnly.Enabled := True;
      if esoSelectedOnly in Options then
        RBSelectedOnly.Checked := True
      else
        RBGlobal.Checked := True
    end else
    begin
      RBGlobal.Checked := True;
      RBSelectedOnly.Enabled := False;
    end;
  end;
end;

{$IFDEF FPC}
initialization
  {$i search.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
