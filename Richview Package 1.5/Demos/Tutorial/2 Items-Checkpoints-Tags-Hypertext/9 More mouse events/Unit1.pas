unit Unit1;

interface

{==============================================================================}
{ This demo shows mouse events, not connected with hypertext IDs               }
{ - OnRVMouseDown, OnRVMouseUp, OnRVDblClick, OnRVRightClick;                  }
{------------------------------------------------------------------------------}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, RVScroll, RichView, RVStyle, StdCtrls;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    panMouseDown: TPanel;
    panMouseUp: TPanel;
    panRightClick: TPanel;
    panDblClick: TPanel;
    RVStyle1: TRVStyle;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RichView1RVMouseDown(Sender: TRichView; Button: TMouseButton;
      Shift: TShiftState; ItemNo, X, Y: Integer);
    procedure RichView1RVMouseUp(Sender: TRichView; Button: TMouseButton;
      Shift: TShiftState; ItemNo, X, Y: Integer);
    procedure RichView1RVDblClick(Sender: TRichView; ClickedWord: String;
      Style: Integer);
    procedure RichView1RVRightClick(Sender: TRichView; ClickedWord: String;
      Style, X, Y: Integer);
  private
    { Private declarations }
    function MouseInfo(Button: TMouseButton; Shift: TShiftState; ItemNo, X, Y: Integer):String;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var ico: TIcon;
begin
  RichView1.AddNL('More mouse events',1,1);
  RichView1.AddTextNL('There are some more mouse events in RichView'#13+
                      'Left/Right/Double - click in this Window.',0,0,0);

  ico := TIcon.Create;
  ico.Assign(Image1.Picture.Graphic);
  RichView1.AddPictureEx('Notebook image',ico,1, rvvaMiddle);
  RichView1.Add(' - example of image',0);

  RichView1.Format;
end;

function TForm1.MouseInfo(Button: TMouseButton; Shift: TShiftState; ItemNo, X, Y: Integer): String;
var ButtonStr, Word: String;
    Dummy: Integer;
begin
  ButtonStr := '';
  case Button of
    mbLeft:
      ButtonStr := 'Left button';
    mbRight:
      ButtonStr := 'Right button';
    mbMiddle:
      ButtonStr := 'Middle button';
  end;

  if ItemNo<>-1 then begin
    RichView1.GetWordAt(X,Y, Dummy,Word);
    Result := Format('%s at (%d,%d), at item #%d, at word "%s"',
                     [ButtonStr, X,Y, ItemNo, Word]);
    end
  else
    Result := Format('%s at (%d,%d) - no item at this position',
                     [ButtonStr, X,Y]);
end;

procedure TForm1.RichView1RVMouseDown(Sender: TRichView;
  Button: TMouseButton; Shift: TShiftState; ItemNo, X, Y: Integer);

begin
  panMouseDown.Caption := 'MouseDown: '+MouseInfo(Button, Shift, ItemNo, X, Y);
end;

procedure TForm1.RichView1RVMouseUp(Sender: TRichView;
  Button: TMouseButton; Shift: TShiftState; ItemNo, X, Y: Integer);
begin
  panMouseUp.Caption := 'MouseUp: '+MouseInfo(Button, Shift, ItemNo, X, Y);
end;

procedure TForm1.RichView1RVDblClick(Sender: TRichView;
  ClickedWord: String; Style: Integer);
begin
  panDblClick.Caption := Format('DoubleClick: at word="%s", at item having style=%d',
                                  [ClickedWord,Style]);
end;

// This event is obsolete. Use OnRVMouseUp instead
procedure TForm1.RichView1RVRightClick(Sender: TRichView; ClickedWord: String; Style, X, Y: Integer);
begin
  panRightClick.Caption := Format('RightClick: at (%d,%d), at word="%s", at item having style=%d',
                                  [X,Y,ClickedWord,Style]);
end;

end.
