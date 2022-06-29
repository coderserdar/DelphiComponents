unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle, RVScroll, RichView, RVEdit, CRVFData;

type
  TForm1 = class(TForm)
    RVStyle1: TRVStyle;
    RichViewEdit1: TRichViewEdit;
    procedure FormCreate(Sender: TObject);
    procedure RVStyle1DrawStyleText(Sender: TRVStyle; const s: String;
      Canvas: TCanvas; StyleNo, SpaceBefore, Left, Top, Width,
      Height: Integer; DrawState: TRVTextDrawStates;
      var DoDefault: Boolean);
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
  with RichViewEdit1 do begin
    Clear;
    AddNL('M',1,0);
    Add('=||m',0);
    Add('i,j',3);
    Add('||',0);
    // Note: rvoTagsArePChars in Options
    AddTag('n',2, Integer(StrNew('m'))); // double scripts are only as example
                                         // here. They are too imperfect -
                                         // assume upperscript text more narrow
                                         // (or at least not very wider)
                                         // than superscript
    Format;
  end;
end;

procedure TForm1.RVStyle1DrawStyleText(Sender: TRVStyle; const s: String;
  Canvas: TCanvas; StyleNo, SpaceBefore, Left, Top, Width, Height: Integer;
  DrawState: TRVTextDrawStates; var DoDefault: Boolean);
var uppertext: PChar;
begin
  case StyleNo of
    1: // dot
      begin
        // displaying small circle in the right top corner of item
        // and allowing default drawing of item text
        if (rvtsItemEnd in DrawState) and (Length(s)>0) then begin
          Canvas.Pen.Color := Sender.TextStyles[1].Color;
          Canvas.Ellipse(Left+Width-2,Top-2,Left+Width+2,Top+2);
        end;
      end;
    2: // double scripts
      begin
        // displaying tag string as superscript
        // and allowing default drawing of item text (subscript)
        if (rvtsItemStart in DrawState) and (Length(s)>0) then begin
          uppertext := PChar(TCustomRVFormattedData(Sender.RVData).GetItemTag(Sender.ItemNo));
          if uppertext<>nil then
            Canvas.TextOut(Left,Top-Height+5, uppertext);
        end;
      end;
  end;
end;

end.
