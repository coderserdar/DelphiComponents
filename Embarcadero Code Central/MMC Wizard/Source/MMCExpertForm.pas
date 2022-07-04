unit MMCExpertForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ToolsAPI, ActnList;

type
  TfmSnapinWizard = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    btnClose: TButton;
    Image1: TImage;
    TabSheet2: TTabSheet;
    StaticText1: TStaticText;
    edSnapinDescription: TEdit;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    edSnapinName: TEdit;
    StaticText4: TStaticText;
    TabSheet3: TTabSheet;
    StaticText5: TStaticText;
    edSnapinProvider: TEdit;
    StaticText6: TStaticText;
    btnBack: TButton;
    btnNext: TButton;
    ActionList1: TActionList;
    actBack: TAction;
    actNext: TAction;
    actClose: TAction;
    TabSheet4: TTabSheet;
    StaticText7: TStaticText;
    edStaticScopeItem: TEdit;
    StaticText8: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure actBackExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    function OnLastPage : boolean;
  protected
    procedure UpdateActions; override;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmSnapinWizard: TfmSnapinWizard;

implementation

uses MMCExpertImpl;

{$R *.DFM}

procedure TfmSnapinWizard.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  PageControl1Change (PageControl1);
end;

procedure TfmSnapinWizard.UpdateActions;
begin
  actBack.Enabled := PageControl1.ActivePageIndex > 0;
  actNext.Enabled := not OnLastPage;
end;

procedure TfmSnapinWizard.actBackExecute(Sender: TObject);
begin
  PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;
  PageControl1Change (PageControl1);
end;

procedure TfmSnapinWizard.actNextExecute(Sender: TObject);
begin
  PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;
  PageControl1Change (PageControl1);
end;

procedure TfmSnapinWizard.actCloseExecute(Sender: TObject);
begin
  if OnLastPage then
    ModalResult := mrOK
  else
    ModalResult := mrCancel
end;

procedure TfmSnapinWizard.PageControl1Change(Sender: TObject);
var
  i : Integer;
  ctrl : TControl;
  wctrl : TWinControl;
begin
  if OnLastPage then
    actClose.Caption := 'Done'
  else
    actClose.Caption := 'Cancel';

  ActiveControl := PageControl1;

  for i := 0 to PageControl1.ActivePage.ControlCount - 1 do
  begin
    ctrl := PageControl1.ActivePage.Controls [i];
    if ctrl is TWinControl then
    begin
      wctrl := TWinControl (ctrl);
      if wctrl.TabStop then
      begin
        ActiveControl := wctrl;
        break
      end
    end
  end
end;

function TfmSnapinWizard.OnLastPage: boolean;
begin
  result := PageControl1.ActivePageIndex = PageControl1.PageCount - 1;
end;

end.
