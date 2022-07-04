unit unit_Edit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls,
  CHEdit;

type
  TfrmCHEdit = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    Label1: TLabel;
    CHEdit1: TCHEdit;
    Label2: TLabel;
    CHEdit2: TCHEdit;
    Label3: TLabel;
    CHEdit3: TCHEdit;
    Label4: TLabel;
    CHEdit4: TCHEdit;
    Label5: TLabel;
    CHEdit5: TCHEdit;
    Label21: TLabel;
    CHEdit17: TCHEdit;
    Label6: TLabel;
    Label7: TLabel;
    CHEdit10: TCHEdit;
    Label12: TLabel;
    CHEdit9: TCHEdit;
    Label11: TLabel;
    CHEdit8: TCHEdit;
    Label10: TLabel;
    CHEdit7: TCHEdit;
    Label9: TLabel;
    CHEdit6: TCHEdit;
    Label8: TLabel;
    Label13: TLabel;
    CHEdit11: TCHEdit;
    Label14: TLabel;
    CHEdit12: TCHEdit;
    Label15: TLabel;
    CHEdit13: TCHEdit;
    Label16: TLabel;
    CHEdit14: TCHEdit;
    Label17: TLabel;
    CHEdit15: TCHEdit;
    Label18: TLabel;
    CHEdit16: TCHEdit;
    Label19: TLabel;
    Label20: TLabel;
    procedure close1Click(Sender: TObject);
    procedure CHEdit6KeyAltPress(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CHEdit7KeyEnterPress(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CHEdit8KeyESCPress(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CHEdit9KeyShiftPress(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CHEdit10KeyStrgPress(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHEdit: TfrmCHEdit;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHEdit.close1Click(Sender: TObject);
begin
  Close;
end;


procedure TfrmCHEdit.CHEdit6KeyAltPress(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Application.MessageBox('You pressed the ALT button.' + #13 +
    'The event "OnKeyAltPress" activated this dialog', 'OnKeyAltPress',
    MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
end;

procedure TfrmCHEdit.CHEdit7KeyEnterPress(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Application.MessageBox('You pressed the ENTER button.' + #13 +
    'The event "OnKeyEnterPress" activated this dialog', 'OnKeyEnterPress',
    MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
end;

procedure TfrmCHEdit.CHEdit8KeyESCPress(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Application.MessageBox('You pressed the ESC button.' + #13 +
    'The event "OnKeyESCPress" activated this dialog', 'OnKeyESCPress',
    MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
end;

procedure TfrmCHEdit.CHEdit9KeyShiftPress(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Application.MessageBox('You pressed the SHIFT button.' + #13 +
    'The event "OnKeyShiftPress" activated this dialog', 'OnKeyShiftPress',
    MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
end;

procedure TfrmCHEdit.CHEdit10KeyStrgPress(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Application.MessageBox('You pressed the STRG button.' + #13 +
    'The event "OnKeyStrgPress" activated this dialog', 'OnKeyStrgPress',
    MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
end;

procedure TfrmCHEdit.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

end.
