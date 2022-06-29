unit Demo1Frm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle, RVScroll, RichView, StdCtrls, Menus;

type
  TfrmDemo1 = class(TForm)
    lst: TListBox;
    rv: TRichView;
    pm: TPopupMenu;
    mitShowCP: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure rvCheckpointVisible(Sender: TRichView;
      CheckpointData: TCheckpointData);
    procedure lstDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pmPopup(Sender: TObject);
    procedure mitShowCPClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation
uses MainFrm;
{$R *.DFM}
{-----------------------------------------------------------}
procedure TfrmDemo1.FormCreate(Sender: TObject);
var SearchRec: TSearchRec;
begin
  lst.Items.BeginUpdate;
  rv.AddNL('When you scroll document to specified file, corresponded item in '   +
           'list box is highlighted.',sncomHeading,0);
  rv.AddNL('Double-click on the right listbox to scroll document to file.',
           sncomHeading,0);
  rv.AddNL('Right-click to show/hide "checkpoints".',
           sncomHeading,0);
  if FindFirst(ExtractFilePath(Application.ExeName)+'*.pas',
               0,SearchRec) = 0 then
    repeat
      lst.Items.Add(SearchRec.Name);
      // Marking next added item with checkpoint
      rv.AddNamedCheckpointEx(SearchRec.Name, True);
      // Adding name of file
      rv.AddNL(SearchRec.Name,sncomKeyword,3);
      // Adding text from file.
      // Last parameter = True, so all text will be loaded as one
      // paragraph, and displayed in the single frame
      rv.LoadText(ExtractFilePath(Application.ExeName)+SearchRec.Name, sncomNormal, 2, True);
    until FindNext(SearchRec)<>0;
  FindClose(SearchRec);
  lst.Items.EndUpdate;
  rv.Format;
end;
{-----------------------------------------------------------}
procedure TfrmDemo1.rvCheckpointVisible(Sender: TRichView;
  CheckpointData: TCheckpointData);
begin
  if CheckpointData=nil then
    lst.ItemIndex := -1
  else
    lst.ItemIndex := rv.GetCheckpointNo(CheckpointData);
end;
{-----------------------------------------------------------}
procedure TfrmDemo1.lstDblClick(Sender: TObject);
begin
  if lst.ItemIndex=-1 then exit;
  rv.ScrollTo(rv.GetCheckpointY(lst.ItemIndex));
end;
{-----------------------------------------------------------}
procedure TfrmDemo1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
end;
{-----------------------------------------------------------}
procedure TfrmDemo1.pmPopup(Sender: TObject);
begin
  mitShowCP.Checked := rvoShowCheckpoints in rv.Options;
end;
{-----------------------------------------------------------}
procedure TfrmDemo1.mitShowCPClick(Sender: TObject);
begin
  if rvoShowCheckpoints in rv.Options then
    rv.Options := rv.Options - [rvoShowCheckpoints]
  else
    rv.Options := rv.Options + [rvoShowCheckpoints];
  rv.Invalidate;
end;

end.
