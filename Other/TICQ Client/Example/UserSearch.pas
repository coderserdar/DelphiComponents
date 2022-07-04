(*

eICQ: the free ICQ for Microsoft(tm) Windows(tm)

Copyright 2003-2004 eICQ ICQ project,
all portions of this codebase are copyrighted to the people
listed in contributors.txt.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

unit UserSearch;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ICQWorks, Menus, SysUtils;

type
  TUserSearchForm = class(TForm)
    GroupBox1: TGroupBox;
    SearchByNumberRadio: TRadioButton;
    NumberEdit: TEdit;
    GroupBox2: TGroupBox;
    SearchByEmailRadio: TRadioButton;
    EmailEdit: TEdit;
    GroupBox3: TGroupBox;
    SearchByNameRadio: TRadioButton;
    NickEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    FirstEdit: TEdit;
    LastEdit: TEdit;
    Label3: TLabel;
    Button1: TButton;
    GroupBox4: TGroupBox;
    ListView1: TListView;
    StatusBar1: TStatusBar;
    GroupBox5: TGroupBox;
    RandomSearchRadio: TRadioButton;
    RandomComboBox: TComboBox;
    Label4: TLabel;
    AdvancedButton: TButton;
    UserSearchPopup: TPopupMenu;
    AddToList1: TMenuItem;
    UsersInfo1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure SearchByEmailRadioClick(Sender: TObject);
    procedure SearchByNumberRadioClick(Sender: TObject);
    procedure SearchByNameRadioClick(Sender: TObject);
    procedure NumberEditChange(Sender: TObject);
    procedure EmailEditChange(Sender: TObject);
    procedure NickEditChange(Sender: TObject);
    procedure RandomSearchRadioClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AdvancedButtonClick(Sender: TObject);
    procedure UserSearchPopupPopup(Sender: TObject);
    procedure UsersInfo1Click(Sender: TObject);
    procedure AddToList1Click(Sender: TObject);
    procedure RandomComboBoxChange(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    { Public declarations }
  end;

var
  UserSearchForm: TUserSearchForm;

implementation
uses
  Main, UserSearchWP, AddUser;

{$R *.dfm}

procedure TUserSearchForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params); //Don't ever forget to do this!!!
  Params.WndParent := GetDesktopWindow;
end;

procedure TUserSearchForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  UserSearchForm := nil;
end;

procedure TUserSearchForm.Button1Click(Sender: TObject);
begin
  if NumberEdit.Text <> '' then
    if (not MainForm.ICQClient1.LoggedIn) or (not MainForm.ValidateRange(NumberEdit.Text, 2)) then
    begin
      MessageBox(Self.Handle,'The messaging protocol reported an error initiating the search. Please correct the fault and try again.','Search',MB_OK);
      Exit;
    end;
  if UserSearchWPForm.Showing then
    MainForm.ICQClient1.SearchWhitePages(
      UserSearchWPForm.FirstNameEdit.Text,
      UserSearchWPForm.LastNameEdit.Text,
      UserSearchWPForm.NickNameEdit.Text,
      UserSearchWPForm.EmailEdit.Text,
      UserSearchWPForm.GetAgeMin,
      UserSearchWPForm.GetAgeMax,
      UserSearchWPForm.GetGender,
      UserSearchWPForm.LanguageCB.Items.Strings[UserSearchWPForm.LanguageCB.ItemIndex],
      UserSearchWPForm.CityEdit.Text,
      UserSearchWPForm.CountryCB.Items.Strings[UserSearchWPForm.CountryCB.ItemIndex],
      UserSearchWPForm.CompanyEdit.Text,
      UserSearchWPForm.DepartmentEdit.Text,
      UserSearchWPForm.PositionEdit.Text,
      UserSearchWPForm.OccupationCB.Items.Strings[UserSearchWPForm.OccupationCB.ItemIndex],
      UserSearchWPForm.OrganizationCB.Items.Strings[UserSearchWPForm.OrganizationCB.ItemIndex],
      UserSearchWPForm.OrganizationKeyWordsEdit.Text,
      UserSearchWPForm.PastAffilCB.Items.Strings[UserSearchWPForm.PastAffilCB.ItemIndex],
      UserSearchWPForm.PastAffilKeyWordsEdit.Text,
      UserSearchWPForm.KeyWordEdit.Text,
      UserSearchWPForm.OnlineCB.Checked)
  else if SearchByEmailRadio.Checked then
    MainForm.ICQClient1.SearchByMail(EmailEdit.Text)
  else if SearchByNumberRadio.Checked then
    MainForm.ICQClient1.SearchByUIN(StrToInt64(NumberEdit.Text))
  else if SearchByNameRadio.Checked then
    MainForm.ICQClient1.SearchByName(FirstEdit.Text, LastEdit.Text, NickEdit.Text, '')
  else if RandomSearchRadio.Checked then
    MainForm.ICQClient1.SearchRandom(RandGroups[RandomComboBox.ItemIndex + 1].Ident)
  else
    Exit;
  Button1.Caption := 'Searching...';
  StatusBar1.Panels[0].Text := 'Searching...';
  ListView1.Items.Clear;
end;

procedure TUserSearchForm.SearchByNumberRadioClick(Sender: TObject);
begin
  SearchByEmailRadio.Checked := False;
  SearchByNameRadio.Checked := False;
  RandomSearchRadio.Checked := False;  
end;

procedure TUserSearchForm.SearchByEmailRadioClick(Sender: TObject);
begin
  SearchByNumberRadio.Checked := False;
  SearchByNameRadio.Checked := False;
  RandomSearchRadio.Checked := False;
end;

procedure TUserSearchForm.SearchByNameRadioClick(Sender: TObject);
begin
  SearchByNumberRadio.Checked := False;
  SearchByEmailRadio.Checked := False;
  RandomSearchRadio.Checked := False;
end;

procedure TUserSearchForm.RandomSearchRadioClick(Sender: TObject);
begin
  SearchByNumberRadio.Checked := False;
  SearchByEmailRadio.Checked := False;
  SearchByNameRadio.Checked := False;
end;

procedure TUserSearchForm.NumberEditChange(Sender: TObject);
begin
  SearchByNumberRadio.Checked := True;
  SearchByEmailRadio.Checked := False;
  SearchByNameRadio.Checked := False;
  RandomSearchRadio.Checked := False;
end;

procedure TUserSearchForm.EmailEditChange(Sender: TObject);
begin
  SearchByNumberRadio.Checked := False;
  SearchByEmailRadio.Checked := True;
  SearchByNameRadio.Checked := False;
  RandomSearchRadio.Checked := False;
end;

procedure TUserSearchForm.NickEditChange(Sender: TObject);
begin
  SearchByNumberRadio.Checked := False;
  SearchByEmailRadio.Checked := False;
  SearchByNameRadio.Checked := True;
  RandomSearchRadio.Checked := False;
end;

procedure TUserSearchForm.FormCreate(Sender: TObject);
var
  i: Word;
begin
  for i := Low(RandGroups) to High(RandGroups) do
    RandomComboBox.Items.Add(RandGroups[i].Value);
  RandomComboBox.ItemIndex := 0;

  MainForm.OnlyNumbers(NumberEdit);
end;

procedure TUserSearchForm.AdvancedButtonClick(Sender: TObject);
begin
  UserSearchWPForm.Show;
end;

procedure TUserSearchForm.UserSearchPopupPopup(Sender: TObject);
begin
  if ListView1.Selected = nil then
  begin
    UserSearchPopup.Items[0].Enabled := False;
    UserSearchPopup.Items[1].Enabled := False;
  end
  else
  begin
    UserSearchPopup.Items[0].Enabled := True;
    UserSearchPopup.Items[1].Enabled := True;
  end;
end;

procedure TUserSearchForm.UsersInfo1Click(Sender: TObject);
begin
  if ListView1.Selected = nil then Exit;
  MainForm.DoCreateInfoQuery(ListView1.Selected.SubItems[3]);
end;

procedure TUserSearchForm.AddToList1Click(Sender: TObject);
var
  dwUIN: DWORD;
begin
  //ListView Filling
  if ListView1.Selected = nil then Exit;
 {
  for i := 0 to MainForm.ICQClient1.ContactList.Count -1 do
    if  ListView1.Selected.SubItems[3] = MainForm.ICQClient1.ContactList.Strings[i]
     then Exit;
  }
  dwUIN := StrToInt64(ListView1.Selected.SubItems[3]);

  with TAddUserForm.Create(Self) do
  begin
    FUIN := dwUIN;
    Caption := Format('Add Contact (%u)', [dwUIN]);
    dlgHandle := Self.Handle;
    FNick := ListView1.Selected.Caption;
    mNick.Lines.Text := FNick;
    EnableWindow(Self.Handle, False);
    ShowWindow(Self.Handle, SW_SHOWNORMAL);
    Show;
  end;
end;

procedure TUserSearchForm.RandomComboBoxChange(Sender: TObject);
begin
  SearchByNumberRadio.Checked := False;
  SearchByEmailRadio.Checked := False;
  SearchByNameRadio.Checked := False;
  RandomSearchRadio.Checked := True;
end;

end.
