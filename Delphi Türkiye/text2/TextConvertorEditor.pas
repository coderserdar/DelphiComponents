unit TextConvertorEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TformTextConvertorEditor = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    btnAdd: TButton;
    btnDelete: TButton;
    btnClear: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    Help: TButton;
    OpenDialog: TOpenDialog;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure ItemClick(Sender: TObject);
  private
    FNoItems: Boolean;
    procedure AddEmpty;
    procedure RemoveEmpty;
    procedure SetButtons;
  public
    property NoItems: Boolean read FNoItems;
  end;

var
  formTextConvertorEditor: TformTextConvertorEditor;

const
  strNoFilesSelected = '<dosya seçilmedi>';
  strCaption: array[False..True] of string = ('Output files', 'Input files');

function EditTextConvertorFiles(List: TStrings; AMode: Boolean): Boolean;

implementation

{$R *.dfm}

function EditTextConvertorFiles(List: TStrings; AMode: Boolean): Boolean;
begin
  with TformTextConvertorEditor.Create(Application) do
       try
         Caption := strCaption[AMode];
         if List.Count = 0 then AddEmpty
         else ListBox1.Items.Assign(List);
         SetButtons;
         Result := ShowModal = mrOk;
         if Result then
         begin
            RemoveEmpty;
            List.Assign(ListBox1.Items);
         end;
       finally
         Free;
       end;
end;

procedure TformTextConvertorEditor.AddEmpty;
begin
   if not FNoItems then
   begin
      ListBox1.Items.Add(strNoFilesSelected);
      FNoItems := True;
   end;
end;

procedure TformTextConvertorEditor.btnAddClick(Sender: TObject);
begin
   with OpenDialog, ListBox1.Items do
        if Execute then
        begin
           if IndexOf(FileName) = -1 then
           begin
              RemoveEmpty;
              Add(FileName);
           end;
           SetButtons;
        end;
end;

procedure TformTextConvertorEditor.btnClearClick(Sender: TObject);
begin
   with ListBox1 do
   begin
      Clear;
      AddEmpty;
   end;
   SetButtons;
end;

procedure TformTextConvertorEditor.btnDeleteClick(Sender: TObject);
var
  I: Integer;
begin
   with ListBox1 do
   begin
      for I := Items.Count - 1 downto 0 do
          if Selected[I] then Items.Delete(I);
      if Items.Count = 0 then AddEmpty;
   end;
   SetButtons;
end;

procedure TformTextConvertorEditor.ItemClick(Sender: TObject);
begin
  SetButtons;
end;

procedure TformTextConvertorEditor.RemoveEmpty;
begin
   if FNoItems then
   begin
      ListBox1.Items.Delete(0);
      FNoItems := False;
   end;
end;

procedure TformTextConvertorEditor.SetButtons;
var
  I: Integer;
begin
   btnClear.Enabled := not NoItems;
   if NoItems then btnDelete.Enabled := False
   else with ListBox1 do
   begin
      for I := Items.Count - 1 downto 0 do
          if Selected[I] then Break;
      btnDelete.Enabled := I <> -1;
   end;
end;

end.
