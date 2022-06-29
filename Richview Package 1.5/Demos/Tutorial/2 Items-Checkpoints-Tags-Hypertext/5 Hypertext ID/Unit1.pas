unit Unit1;

interface

{==============================================================================}
{ Demo of basic using of hypertext                                             }
{ In this demo were modified styles: RVStyle1.TextStyle[4] and                 }
{ RVStyle.TextStyle[5]                                                         }
{ Setting RVStyle.TextStyle[i].Jump to True turns this text style into         }
{ hypertext style.                                                             }
{ Properties of text styles affecting hypertext appearance:                    }
{ - HoverColor (color of hypertext under mouse (clNone for not changing)       }
{ - HoverBackColor (color of hypertext background under mouse (clNone for      }
{   transparent)                                                               }
{ - JumpCursor                                                                 }
{------------------------------------------------------------------------------}
{ Key events and properties:                                                   }
{ - OnJump, OnRVMouseMove                                                      }
{ - FirstJumpNo                                                                }
{------------------------------------------------------------------------------}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, RVStyle, RVScroll, RichView;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    RVStyle1: TRVStyle;
    Label1: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure RichView1Jump(Sender: TObject; id: Integer);
    procedure RichView1RVMouseMove(Sender: TObject; id: Integer);
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
begin
  // RVStyle1.TextStyles[4].Jump = RVStyle1.TextStyles[5].Jump = True
  // This causes these styles represent hypertext 
  RichView1.AddNL('Hypertext',1,1);
  RichView1.AddNL('Some text styles can be chosen as hypertext styles. ',0,0);
  RichView1.AddNL('Like this one.',4,-1);
  RichView1.AddNL(' You can have as many hypertext styles as you want.  ',0,-1);
  RichView1.AddNL('Here is one more.',5,-1);
  RichView1.Format;

  {
    The basic method to use hypertext is "hypertext IDs".
    All hypertext jumps are numbered sequentially (0,1,...) from top of
    document to bottom. These numbers are called "hypertext IDs".
    Hypertext id is passed in OnJump and OnRVMouseMove events.
  }
  {
    More correct, jumps are numbered as FirstJumpNo, FirstJumpNo+1,
    FirstJumpNo+2,...
    FirstJumpNo is a property of RichView, 0 by default
  }
end;

procedure TForm1.RichView1Jump(Sender: TObject; id: Integer);
begin
  Panel1.Caption := 'Clicked: '+IntToStr(id);
end;

procedure TForm1.RichView1RVMouseMove(Sender: TObject; id: Integer);
begin
  // id=-1 when mouse leaves hypertext jump area
  if id<>-1 then
    Label1.Caption := IntToStr(id)
  else
    Label1.Caption := '---';
end;

end.
