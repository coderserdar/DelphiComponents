unit Unit1;
{==============================================================================}
{ This demo shows how to add controls to RichView                              }
{==============================================================================}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVStyle, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    RVStyle1: TRVStyle;
    RichView1: TRichView;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure DoOnScrollbarChange(Sender: TObject); 
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// This event handler will be assigned to scrollbar's OnChange
procedure TForm1.DoOnScrollbarChange(Sender: TObject);
begin
  Label3.Caption := IntToStr(TScrollbar(Sender).Position);
end;

procedure TForm1.FormCreate(Sender: TObject);
var sb: TScrollbar;
    pan : TPanel;
begin
  RichView1.Clear;
  RichView1.AddNL('Example of adding controls', 1, 1);

  // Adding "break" - horizontal line
  RichView1.AddBreakEx(3, rvbsLine, clRed);
  // 1st parameter - line width (AddBreak method adds break with width=1)
  // 2nd parameter - reserved, must be set to rvbsLine
  // 3rd parameter - color; if set to clNone, "break" will have color of the
  // 0th text style (AddBreak method adds break with color=clNone)

  //-------------------------------------------//
  //    Example 1: adding controls from form:  //
  //-------------------------------------------//
  // Adding combobox
  RichView1.AddNL('Combobox:', 0, 0);
  // AddControlEx removes control from its current parent and insert into RichView
  // Just one line of code:
  RichView1.AddControlEx('', Combobox1, 1, rvvaBaseline);

  //-------------------------------------------//
  //    Example 1: adding controls created at  //
  //    run-time:                              //
  //-------------------------------------------//
  RichView1.AddNL('Panel with scrollbar:', 0, 1);
  // Adding panel with scrollbar
  pan := TPanel.Create(nil); // we can set NIL to Owner because this panel will be freed by RichView
  pan.Caption := '';
  pan.Width := 100;
  pan.Height := 60;
  sb := TScrollbar.Create(pan); // panel will free scrollbar
  sb.Parent := pan;
  sb.Min := -10;
  sb.Max := 10;
  sb.SetBounds(10,20,80,20);
  sb.OnChange := DoOnScrollbarChange;
  RichView1.AddControlEx('', pan, -1, rvvaMiddle);

  RichView1.AddBreakEx(3, rvbsLine, clRed);

  RichView1.Format;

  // About AddControlEx:
  // Parameters of this method are similar with parameters of AddPictureEx:
  // 1st parameter: name of control. Allows to hold additional text information
  //  together with control. There is no predefined meaning of this
  //  parameter.
  // 2nd parameter: control
  // 3rd parameter: index of paragraph style (-1 to continue paragraph)
  // 4th parameter: vertical align of control.
end;

end.
