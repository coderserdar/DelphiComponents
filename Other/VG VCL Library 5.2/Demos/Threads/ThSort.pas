{ TvgThread component demo }

unit ThSort;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls,
  vgTools;

type
  TThreadSortForm = class(TForm)
    StartBtn: TButton;
    BubbleSortBox: TPaintBox;
    SelectionSortBox: TPaintBox;
    QuickSortBox: TPaintBox;
    Label1: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    thBubble: TvgThread;
    thSelection: TvgThread;
    thQuick: TvgThread;
    procedure BubbleSortBoxPaint(Sender: TObject);
    procedure SelectionSortBoxPaint(Sender: TObject);
    procedure QuickSortBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure thBubbleExecute(Sender: TObject);
    procedure ThreadTerminate(Sender: TObject);
    procedure thSelectionExecute(Sender: TObject);
    procedure thQuickExecute(Sender: TObject);
  private
    ThreadsRunning: Integer;
    procedure DoVisualSwap(Sender: TObject);
    procedure VisualSwap(Thread: TvgThread; Box: TPaintBox; A, B, I, J: Integer);
    procedure RandomizeArrays;
  public
    procedure PaintArray(Box: TPaintBox; const A: array of Integer);
  end;

var
  ThreadSortForm: TThreadSortForm;

implementation

{$R *.DFM}

procedure PaintLine(Canvas: TCanvas; I, Len: Integer);
begin
  Canvas.PolyLine([Point(0, I * 2 + 1), Point(Len, I * 2 + 1)]);
end;

type
  PSortArray = ^TSortArray;
  TSortArray =  array[0..114] of Integer;

  PSwapData  = ^TSwapData;
  TSwapData  = record
     FA, FB, FI, FJ: Integer;
     FBox: TPaintBox;
  end;


var
  ArraysRandom: Boolean;
  BubbleSortArray, SelectionSortArray, QuickSortArray: TSortArray;

procedure TThreadSortForm.PaintArray(Box: TPaintBox; const A: array of Integer);
var
  I: Integer;
begin
  with Box do
  begin
    Canvas.Pen.Color := clRed;
    for I := Low(A) to High(A) do PaintLine(Canvas, I, A[I]);
  end;
end;

procedure TThreadSortForm.BubbleSortBoxPaint(Sender: TObject);
begin
  PaintArray(BubbleSortBox, BubbleSortArray);
end;

procedure TThreadSortForm.SelectionSortBoxPaint(Sender: TObject);
begin
  PaintArray(SelectionSortBox, SelectionSortArray);
end;

procedure TThreadSortForm.QuickSortBoxPaint(Sender: TObject);
begin
  PaintArray(QuickSortBox, QuickSortArray);
end;

procedure TThreadSortForm.FormCreate(Sender: TObject);
begin
  RandomizeArrays;
end;

procedure TThreadSortForm.RandomizeArrays;
var
  I: Integer;
begin
  if not ArraysRandom then
  begin
    Randomize;
    for I := Low(BubbleSortArray) to High(BubbleSortArray) do
      BubbleSortArray[I] := Random(170);
    SelectionSortArray := BubbleSortArray;
    QuickSortArray := BubbleSortArray;
    ArraysRandom := True;
    Repaint;
  end;
end;

procedure TThreadSortForm.StartBtnClick(Sender: TObject);
begin
  RandomizeArrays;
  ThreadsRunning := 3;

  StartBtn.Enabled := False;

  thBubble.Execute;
  thSelection.Execute;
  thQuick.Execute;
end;

procedure TThreadSortForm.ThreadTerminate(Sender: TObject);
begin
  Dec(ThreadsRunning);
  if ThreadsRunning = 0 then
  begin
    StartBtn.Enabled := True;
    ArraysRandom := False;
  end;
end;

procedure TThreadSortForm.DoVisualSwap(Sender: TObject);
begin
  with PSwapData(Sender)^, FBox do
  begin
    Canvas.Pen.Color := clBtnFace;
    PaintLine(Canvas, FI, FA);
    PaintLine(Canvas, FJ, FB);
    Canvas.Pen.Color := clRed;
    PaintLine(Canvas, FI, FB);
    PaintLine(Canvas, FJ, FA);
  end;
end;

procedure TThreadSortForm.VisualSwap(Thread: TvgThread; Box: TPaintBox; A, B, I, J: Integer);
var
  SD: TSwapData;
begin
  with SD do
  begin
    FA := A;
    FB := B;
    FI := I;
    FJ := J;
    FBox := Box;
  end;
  Thread.SynchronizeEx(DoVisualSwap, @SD);
end;

procedure TThreadSortForm.thBubbleExecute(Sender: TObject);
var
  I, J, T: Integer;
  A: TSortArray absolute BubbleSortArray;
begin
  for I := High(A) downto Low(A) do
    for J := Low(A) to High(A) - 1 do
      if A[J] > A[J + 1] then
      begin
        VisualSwap(thBubble, BubbleSortBox, A[J], A[J + 1], J, J + 1);
        T := A[J];
        A[J] := A[J + 1];
        A[J + 1] := T;
      end;
end;

procedure TThreadSortForm.thSelectionExecute(Sender: TObject);
var
  I, J, T: Integer;
  A: TSortArray absolute SelectionSortArray;
begin
  for I := Low(A) to High(A) - 1 do
    for J := High(A) downto I + 1 do
      if A[I] > A[J] then
      begin
        VisualSwap(thSelection, SelectionSortBox, A[I], A[J], I, J);
        T := A[I];
        A[I] := A[J];
        A[J] := T;
      end;
end;

procedure TThreadSortForm.thQuickExecute(Sender: TObject);
  procedure QuickSort(var A: array of Integer; iLo, iHi: Integer);
  var
    Lo, Hi, Mid, T: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        VisualSwap(thQuick, QuickSortBox, A[Lo], A[Hi], Lo, Hi);
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(A, iLo, Hi);
    if Lo < iHi then QuickSort(A, Lo, iHi);
  end;
var
  A: TSortArray absolute QuickSortArray;
begin
  QuickSort(A, Low(A), High(A));
end;

end.
