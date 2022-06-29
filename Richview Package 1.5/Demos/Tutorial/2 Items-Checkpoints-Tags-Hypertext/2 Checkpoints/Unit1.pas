unit Unit1;

interface

{==============================================================================}
{ This demo shows how to scroll to specified position of document using        }
{ special invisible labels - "checkpoints"                                     }
{ Key methods and properties:                                                  }
{ - AddNamedCheckpoint;                                                        }
{ - GetCheckpointByNo, FindCheckpointByName;                                   }
{ - GetCheckpointY, GetCheckpointYEx;                                          }
{ - ScrollTo                                                                   }
{ - Options (rvoShowCheckpoints)                                               }
{==============================================================================}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RVStyle, RVScroll, RichView;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    RVStyle1: TRVStyle;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
begin
  for i := 1 to 3 do begin
    RichView1.AddNamedCheckpoint('C'+IntToStr(i));
    RichView1.AddFmt('Chapter %d', [i], 1,1);
    for j := 0 to 30 do
      RichView1.AddNL('Bla - bla - bla - bla - bla - bla - bla - bla'+
                      '- bla - bla - bla - bla - bla - bla - bla - bla',0,0);
  end;
  RichView1.AddNamedCheckpoint('END');
  RichView1.Format;
  {
    Comments:

    Checkpoints are not items. They are special additional information,
    associated with any item.
    (in older, freeware versions, checkpoints were items)
    But checkpoint can be added like any other item using AddXXX methods:
    AddNamedCheckpoint, AddCheckpoint, AddNamedCheckpointEx, and some other.

    AddNamedCheckpoint('') == AddCheckpoint

    Checkpoint added with some of these methods will be associated with next
    added item (if no items added after it, checkpoints becomes special
    end-of-text checkpoint which is not associated with any item)

    Do not try to add checkpoints one after another without items between them
    (it's impossible, and causes the exception)
  }
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  // toggles checkpoints visibility
  if Checkbox1.Checked then
    RichView1.Options := RichView1.Options+[rvoShowCheckpoints]
  else
    RichView1.Options := RichView1.Options-[rvoShowCheckpoints];
  RichView1.Invalidate;
end;

procedure TForm1.Button1Click(Sender: TObject);
var CheckpointData: TCheckpointData;
    Y: Integer;
begin
  // GetCheckpointByNo(checkpoint index) - returns value of type TCheckpointData,
  // identifying checkpoint
  CheckpointData := RichView1.GetCheckpointByNo(0);
  // GetCheckpointYEx returns Y coordinate of checkpoint
    Y := RichView1.GetCheckpointYEx(CheckpointData);
  // ScrollTo - scrolls to specified Y coordinate
  RichView1.ScrollTo(Y);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // The same actions, more compact
  with RichView1 do
    ScrollTo(GetCheckpointYEx(GetCheckpointByNo(1)));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  // Even more compact
  with RichView1 do
    ScrollTo(GetCheckpointY(2));
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  // We can use checkpoint name to find it
  with RichView1 do
    ScrollTo(GetCheckpointYEx(FindCheckpointByName('END')));
end;

end.
