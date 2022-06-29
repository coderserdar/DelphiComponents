unit Unit1;

interface

{==============================================================================}
{ This demo shows how to obtain list of checkpoints in document.               }
{ Key methods:                                                                 }
{ - GetFirstCheckpoint, GetNextCheckpoint;                                     }
{ - GetCheckpointInfo.                                                         }
{==============================================================================}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RVStyle, RVScroll, RichView;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    RVStyle1: TRVStyle;
    ListBox1: TListBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var i, j: Integer;
    CheckpointData : TCheckpointData;
    Name: String;
    RaiseEvent: Boolean;
    Tag: Integer;
begin
  // 1. Filling in RichView
  for i := 1 to 3 do begin
    RichView1.AddNamedCheckpoint('C'+IntToStr(i));
    RichView1.AddFmt('Chapter %d', [i], 1,1);
    for j := 0 to 30 do
      RichView1.AddNL('Bla - bla - bla - bla - bla - bla - bla - bla'+
                      '- bla - bla - bla - bla - bla - bla - bla - bla',0,0);
  end;
  RichView1.AddNamedCheckpoint('END');
  RichView1.Format;
  // 2. Filling in list of checkpoints
  CheckpointData :=  RichView1.GetFirstCheckpoint;
  if CheckpointData<>nil then begin
    repeat
      RichView1.GetCheckpointInfo(CheckpointData, Tag, Name, RaiseEvent);
      // Tag and RaiseEvent will be discussed in next demos
      ListBox1.Items.Add(Name);
      CheckpointData := RichView1.GetNextCheckpoint(CheckpointData);
    until CheckpointData=nil;
    ListBox1.ItemIndex := 0;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var CheckpointIndex: Integer;
begin
  CheckpointIndex := ListBox1.ItemIndex;
  if CheckpointIndex=-1 then exit;
  RichView1.ScrollTo(RichView1.GetCheckpointY(CheckpointIndex));
end;

end.
