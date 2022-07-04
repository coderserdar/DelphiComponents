{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29


  Written by Arpad Toth, SignalSoft s.r.o.
  ekosoft@signalsoft.sk
  www.signalsoft.sk

  Allows to save/restore a user filter templates.
}
unit SMDBFltrFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfrmFilterFileDialog = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    gbDescription: TGroupBox;
    edFileName: TEdit;
    lblFileName: TLabel;
    lvTemplate: TListView;
    memoDescription: TMemo;
    img: TImageList;
    lblDescription: TLabel;
    cbProtect: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure lvTemplateClick(Sender: TObject);
    procedure lvTemplateDblClick(Sender: TObject);
    procedure edFileNameChange(Sender: TObject);
    procedure lvTemplateKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    function Execute(var fltFile: string; SaveOpen: Boolean): Boolean;
  end;

var
  frmFilterFileDialog: TfrmFilterFileDialog;

implementation

{$R *.DFM}
uses SMCnst, IniFiles;

var
  Path: string;


function TfrmFilterFileDialog.Execute(var fltFile: string;SaveOpen: Boolean): Boolean;
var
  Templ: TIniFile;
begin
//  Result   := False;

  with TfrmFilterFileDialog.Create(Application) do
    try
      fltFile := '';
      Caption := strTemplate;
      lblFileName.Caption := strFFileName;
      gbDescription.Caption := strFDescription;
      btnOk.Caption := SBtnOk;
      btnCancel.Caption := SBtnCancel;
      cbProtect.Caption := strFProtect;

      if SaveOpen then
      begin
        memoDescription.Color := clWhite;
        memoDescription.Top := 15;
      end
      else
      begin
        memoDescription.Color := clBtnFace;
        memoDescription.Top := 43;
      end;

      memoDescription.ReadOnly := not SaveOpen;
      edFileName.Visible := SaveOpen;
      lblFileName.Visible := SaveOpen;
      cbProtect.Visible := SaveOpen;
      memoDescription.Height := 165 - memoDescription.Top;

      edFileName.Visible := lblFileName.Visible;
      Result := (ShowModal = mrOk);

      if Result then
      begin
        fltFile := Path + edFileName.Text + '.smf';
        if SaveOpen then
        begin
          Templ := TIniFile.Create(fltFile);
          if not Templ.ReadBool('Main', 'Protect', False) then
          begin
            if Templ.ReadString('Main', 'Create', '') = '' then
               Templ.WriteString('Main', 'Create', DateToStr(date))
            else
            begin
              Templ.WriteString('Main', 'Modify', DateToStr(date));
            end;

            Templ.WriteString('Main', 'Description', memoDescription.Lines.Text);
            Templ.WriteBool('Main', 'Protect', cbProtect.Checked);
          end
          else
          begin
            MessageDlg(strFProtectErr, mtError, [mbOK], 0);
            Result := False;
          end;

          Templ.Free;
        end;
      end;
    finally
      Free
    end;
end;

procedure TfrmFilterFileDialog.FormActivate(Sender: TObject);
var
  SearchRec: TSearchRec;
  r: Word;
  Item: TListItem; Ini: TIniFile;

  function IIF(Exp: Boolean; ret1, ret2: Variant): Variant;
  begin
    if Exp then
      Result := ret1
    else
      Result := ret2;
  end;

begin
  Path := ExtractFilePath(ParamStr(0));
  lvTemplate.Items.Clear;
  R := FindFirst(Path + '*.smf', faAnyFile, SearchRec);
  while R = 0 do
  begin
    if (SearchRec.Attr and faDirectory) = 0 then
    begin
      Ini := TIniFile.Create(path + '\' + SearchRec.Name);
      Item := lvTemplate.Items.Add;
      Item.ImageIndex := IIF(Ini.ReadBool('Main', 'Protect', False), 1, 0);
      Item.StateIndex := Item.ImageIndex;
      Item.Caption := ChangeFileExt(SearchRec.Name, '');
      Ini.Free;
    end;
    R := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  memoDescription.Lines.Clear;
  btnOk.Enabled := False;
end;

procedure TfrmFilterFileDialog.lvTemplateClick(Sender: TObject);
var Templ: TIniFile;
begin
  memoDescription.Lines.Clear;
  lblDescription.Caption := '';
  btnOk.Enabled := (lvTemplate.Selected <> nil) or (edFileName.Text <> '');
  if btnOk.Enabled and (lvTemplate.Selected <> nil) then
  begin
    edFileName.Text := lvTemplate.Selected.Caption;
    Templ := TIniFile.Create(Path + lvTemplate.Selected.Caption + '.smf');
    lblDescription.Caption := Format(strFCreate,
                      [Templ.ReadString('Main', 'Create', '')])+#13#10+
                      Format(strFModify, [Templ.ReadString('Main', 'Modify', '')]);
    memoDescription.Lines.Add(Templ.ReadString('Main', 'Description', ''));
    Templ.Free;
  end;
end;

procedure TfrmFilterFileDialog.lvTemplateDblClick(Sender: TObject);
begin
  if btnOk.Enabled then
    ModalResult := mrOk;
end;

procedure TfrmFilterFileDialog.edFileNameChange(Sender: TObject);
begin
  btnOk.Enabled := (edFileName.Text <> '');
end;

procedure TfrmFilterFileDialog.lvTemplateKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  lvTemplateClick(self);
end;

end.
