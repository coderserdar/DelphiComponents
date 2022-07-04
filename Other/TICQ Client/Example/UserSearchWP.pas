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

unit UserSearchWP;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, ICQWorks;

type
  TUserSearchWPForm = class(TForm)
    GroupBox1: TGroupBox;
    FirstNameEdit: TEdit;
    Label1: TLabel;
    EmailEdit: TEdit;
    Label2: TLabel;
    CityEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    lastNameEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    AgeCB: TComboBox;
    CountryCB: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NickNameEdit: TEdit;
    GenderCB: TComboBox;
    LanguageCB: TComboBox;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    CompanyEdit: TEdit;
    DepartmentEdit: TEdit;
    Label11: TLabel;
    GroupBox3: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    PositionEdit: TEdit;
    OccupationCB: TComboBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    KeyWordEdit: TEdit;
    Label17: TLabel;
    OnlineCB: TCheckBox;
    Button1: TButton;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    PastAffilKeyWordsEdit: TEdit;
    Label15: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    OrganizationKeyWordsEdit: TEdit;
    Label14: TLabel;
    PastAffilCB: TComboBox;
    OrganizationCB: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function GetAgeMin: Word;
    function GetAgeMax: Word;
    function GetGender: Byte;
  end;

var
  UserSearchWPForm: TUserSearchWPForm;

implementation
uses
  UserSearch;

{$R *.dfm}

function TUserSearchWPForm.GetAgeMin: Word;
begin
  case AgeCB.ItemIndex of
    1: Result := 18;
    2: Result := 23;
    3: Result := 30;
    4: Result := 40;
    5: Result := 50;
    6: Result := 60;
  else
    Result := 0;
  end;
end;

function TUserSearchWPForm.GetAgeMax: Word;
begin
  case AgeCB.ItemIndex of
    1: Result := 22;
    2: Result := 29;
    3: Result := 39;
    4: Result := 49;
    5: Result := 59;
    6: Result := $2710;
  else
    Result := 0;
  end;
end;

function TUserSearchWPForm.GetGender: Byte;
begin
  case GenderCB.ItemIndex of
    1: Result := GEN_FEMALE;
    2: Result := GEN_MALE;
  else
    Result := 0;
  end;
end;

procedure TUserSearchWPForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TUserSearchWPForm.Button1Click(Sender: TObject);
begin
//  if UserSearchForm.Showing then
  Close;
  UserSearchForm.Button1.Click;
end;

procedure TUserSearchWPForm.FormCreate(Sender: TObject);
var
  i: Word;
begin
  AgeCB.Items.Add('');
  AgeCB.ItemIndex := 0;
  AgeCB.Items.Add('18-22');
  AgeCB.Items.Add('23-29');
  AgeCB.Items.Add('30-39');
  AgeCB.Items.Add('40-49');
  AgeCB.Items.Add('50-59');
  AgeCB.Items.Add('60 and above');

  GenderCB.Items.Add('');
  GenderCB.ItemIndex := 0;
  GenderCB.Items.Add('Female');
  GenderCB.Items.Add('Male');
  
  //Fill in language combobox
  LanguageCB.Items.Add('');
  for i := Low(Languages) to High(Languages) do
    LanguageCB.Items.Add(Languages[i].Value);

  CountryCB.Items.Add('');
  CountryCB.ItemIndex := 0;
  //Fill in country combobox
  for i := Low(Countries) to High(Countries) do
    CountryCB.Items.Add(Countries[i].Value);

  OccupationCB.Items.Add('');
  OccupationCB.ItemIndex := 0;
  //Fill in occupation combobox
  for i := Low(Occupations) to High(Occupations) do
    OccupationCB.Items.Add(Occupations[i].Value);

  PastAffilCB.Items.Add('');
  PastAffilCB.ItemIndex := 0;
  //Fill in past combobox
  for i := Low(Pasts) to High(Pasts) do
    PastAffilCB.Items.Add(Pasts[i].Value);

  OrganizationCB.Items.Add('');
  OrganizationCB.ItemIndex := 0;
  //Fill in affiliation combobox
  for i := Low(Organizations) to High(Organizations) do
    OrganizationCB.Items.Add(Organizations[i].Value);
end;

end.
