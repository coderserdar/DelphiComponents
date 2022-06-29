unit Unit1;

interface

{==============================================================================}
{ This demo shows how checkpoints can generate events when they become visible }
{------------------------------------------------------------------------------}
{ Properties set:                                                              }
{  RichView1.CPEventKind = cpeAsSectionStart                                   }
{  RichView2.CPEventKind = cpeWhenVisible                                      }
{  rvoShowCheckpoints included in Options of both RichViews                    }
{------------------------------------------------------------------------------}
{ Key properties, events and methods:                                          }
{ - CPEventKind                                                                }
{ - OnCheckpointVisible                                                        }
{ - AddNamedCheckpointEx                                                       }
{==============================================================================}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RVStyle, RVScroll, RichView, ExtCtrls;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    RVStyle1: TRVStyle;
    Label1: TLabel;
    lblChapter: TLabel;
    Label2: TLabel;
    RichView2: TRichView;
    Label3: TLabel;
    Label4: TLabel;
    lblFigure: TLabel;
    Image1: TImage;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RichView1CheckpointVisible(Sender: TRichView;
      CheckpointData: TCheckpointData);
    procedure RichView2CheckpointVisible(Sender: TRichView;
      CheckpointData: TCheckpointData);
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
    ico: TIcon;
begin
  // 1. Filling in left RichView
  for i := 1 to 3 do begin
    RichView1.AddNamedCheckpointEx('Chapter '+IntToStr(i),True);
    RichView1.AddFmt('Chapter %d', [i], 1,1);
    for j := 0 to 30 do
      RichView1.AddNL('Bla - bla - bla - bla - bla - bla.',0,0);
  end;
  RichView1.Format;
  // 1. Filling in right RichView
  for i := 1 to 3 do begin
    RichView2.AddNamedCheckpointEx('Figure '+IntToStr(i),True);
    ico := TIcon.Create;
    ico.Assign(Image1.Picture.Graphic);
    RichView2.AddPictureEx('', ico, 1, rvvaBaseline);
    RichView2.AddFmt('Figure %d', [i], 3,1);
    for j := 0 to 30 do
      RichView2.AddNL('Bla - bla - bla - bla - bla - bla.',0,0);
  end;
  RichView2.Format;
  {
    Comments:

    1.
    In this demo we use AddNamedCheckpointEx method.
    It has second parameter - RaiseEvent: Boolean.
    If set to True, RichView will generate event when this checkpoint
    becomes visible

    2.
    Checkpoints with RaiseEvent=True can be displayed with different color
    than other checkpoints.
    Color of "normal" checkpoints: RVStyle.CheckpointColor;
    Color of "RaiseEvent" checkpoints: RVStyle.CheckpointEvColor
  }
end;

procedure TForm1.RichView1CheckpointVisible(Sender: TRichView;
  CheckpointData: TCheckpointData);
var Name: String;
    Tag: Integer;
    RE: Boolean;
begin
  if CheckpointData<>nil then begin
    RichView1.GetCheckpointInfo(CheckpointData, Tag, Name, RE);
    lblChapter.Caption := Name;
  end;
end;

procedure TForm1.RichView2CheckpointVisible(Sender: TRichView;
  CheckpointData: TCheckpointData);
var Name: String;
    Tag: Integer;
    RE: Boolean;
begin
  if CheckpointData<>nil then begin
    RichView2.GetCheckpointInfo(CheckpointData, Tag, Name, RE);
    lblFigure.Caption := Name;
    end
  else
    lblFigure.Caption := '(none)';
end;

end.
