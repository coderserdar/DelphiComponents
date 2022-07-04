///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2008 RRR     //
///////////////////////////////////////

unit DMHTMLText;

{$B-}

interface

uses Windows, Graphics, SysUtils, Math;

type
  THTMLTextPainter=class
  private
    FCanvas: TCanvas;
    FX, FY: integer;
    FText: string;
    FVertical: boolean;
    FUText: string;
    FLeft, FTop: integer;
    FCharCounter: integer;
    FTextLength: integer;
    FNewLineShift: integer;
    FInitialFontSize: integer;
    FSubShift: integer;
    FSupShift: integer;
    FShift: integer;
    FSubSupFontSize: integer;
    FInitialFontName: string[127];
    FWidth, FHeight: integer;
    FCalcSizeMode: boolean;
    procedure DrawChar(C: char);
    procedure CheckFormat;
    procedure CheckEntities;
    procedure CheckTags;
    function CheckTag(const Tag: string): boolean;
    procedure NewLine;
    procedure UpdateFont;
  public
    constructor Create(C: TCanvas; const T: string; X, Y: integer; V: boolean);
    procedure DrawText;
    procedure CalculateSize;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
  end;

  TEntity=packed record // represents HTML named entity
    Html: string[9];
    Text: char;
    IsSym: boolean;
    Repl: string[3];
  end;
  
const
  Entities: array[0..100] of TEntity = 
  (
  (Html: 'amp'; Text: '&'; IsSym: false),
  (Html: 'gt'; Text: '>'; IsSym: false),
  (Html: 'lt'; Text: '<'; IsSym: false),
  (Html: 'nbsp'; Text: ' '; IsSym: false),
  (Html: 'quot'; Text: '"'; IsSym: false),
// small greek
  (Html: 'alpha'; Text: 'a'; IsSym: true),
  (Html: 'beta'; Text: 'b'; IsSym: true),
  (Html: 'chi'; Text: 'c'; IsSym: true),
  (Html: 'delta'; Text: 'd'; IsSym: true),
  (Html: 'epsilon'; Text: 'e'; IsSym: true), // no 'fi' here!
  (Html: 'gamma'; Text: 'g'; IsSym: true),
  (Html: 'eta'; Text: 'h'; IsSym: true),
  (Html: 'iota'; Text: 'i'; IsSym: true),
  (Html: 'phi'; Text: 'j'; IsSym: true),
  (Html: 'kappa'; Text: 'k'; IsSym: true),
  (Html: 'lambda'; Text: 'l'; IsSym: true),
  (Html: 'mu'; Text: 'm'; IsSym: true),
  (Html: 'nu'; Text: 'n'; IsSym: true),
  (Html: 'omicron'; Text: 'o'; IsSym: true),
  (Html: 'pi'; Text: 'p'; IsSym: true),
  (Html: 'theta'; Text: 'q'; IsSym: true),
  (Html: 'rho'; Text: 'r'; IsSym: true),
  (Html: 'sigma'; Text: 's'; IsSym: true),
  (Html: 'tau'; Text: 't'; IsSym: true),
  (Html: 'upsilon'; Text: 'u'; IsSym: true),
  (Html: 'omega'; Text: 'w'; IsSym: true),
  (Html: 'xi'; Text: 'x'; IsSym: true),
  (Html: 'psi'; Text: 'y'; IsSym: true),
  (Html: 'zeta'; Text: 'z'; IsSym: true),
// capital greek
  (Html: 'Alpha'; Text: 'A'; IsSym: true),
  (Html: 'Beta'; Text: 'B'; IsSym: true),
  (Html: 'Chi'; Text: 'C'; IsSym: true),
  (Html: 'Delta'; Text: 'D'; IsSym: true),
  (Html: 'Epsilon'; Text: 'E'; IsSym: true), 
  (Html: 'Phi'; Text: 'F'; IsSym: true), // 'Fi' here!!! as in IE
  (Html: 'Gamma'; Text: 'G'; IsSym: true),
  (Html: 'Eta'; Text: 'H'; IsSym: true),
  (Html: 'Iota'; Text: 'I'; IsSym: true),
  (Html: 'Kappa'; Text: 'K'; IsSym: true),
  (Html: 'Lambda'; Text: 'L'; IsSym: true),
  (Html: 'Mu'; Text: 'M'; IsSym: true),
  (Html: 'Nu'; Text: 'N'; IsSym: true),
  (Html: 'Omicron'; Text: 'O'; IsSym: true),
  (Html: 'Pi'; Text: 'P'; IsSym: true),
  (Html: 'Theta'; Text: 'Q'; IsSym: true),
  (Html: 'Rho'; Text: 'R'; IsSym: true),
  (Html: 'Sigma'; Text: 'S'; IsSym: true),
  (Html: 'Tau'; Text: 'T'; IsSym: true),
  (Html: 'Upsilon'; Text: 'U'; IsSym: true),
  (Html: 'Omega'; Text: 'W'; IsSym: true),
  (Html: 'Xi'; Text: 'X'; IsSym: true),
  (Html: 'Psi'; Text: 'Y'; IsSym: true),
  (Html: 'Zeta'; Text: 'Z'; IsSym: true), // total 53!
// other symbols (ISO-1, math - ONLY AVAILABLE in Symbol font)
{todo : it is possible to replace some symbols by more exactly corresponded
chars from MS Sans Serif font used in DM2003 UI! This correspondence 
however may be broken in ANOTHER font.}
  (Html: 'forall'; Text: #$22; IsSym: true; Repl: 'A'),
  (Html: 'exist'; Text: #$24; IsSym: true; Repl: 'E'),
  (Html: 'perp'; Text: #$5E; IsSym: true; Repl: '_|_'),
  (Html: 'prime'; Text: #$A2; IsSym: true; Repl: ''''),
  (Html: 'le'; Text: #$A3; IsSym: true; Repl: '<='),
  (Html: 'infin'; Text: #$A5; IsSym: true; Repl: 'oo'),
  (Html: 'harr'; Text: #$AB; IsSym: true; Repl: '<->'),
  (Html: 'larr'; Text: #$AC; IsSym: true; Repl: '<-'),
  (Html: 'uarr'; Text: #$AD; IsSym: true; Repl: '^'),
  (Html: 'rarr'; Text: #$AE; IsSym: true; Repl: '->'),
  (Html: 'darr'; Text: #$AF; IsSym: true),
  (Html: 'deg'; Text: #$B0; IsSym: true; Repl: '°'), // Repl -?!
  (Html: 'plusmn'; Text: #$B1; IsSym: true; Repl: '+/-'),
  (Html: 'Prime'; Text: #$B2; IsSym: true; Repl: '"'),
  (Html: 'ge'; Text: #$B3; IsSym: true; Repl: '>='),
  (Html: 'times'; Text: #$B4; IsSym: true; Repl: 'x'),
  (Html: 'prop'; Text: #$B5; IsSym: true; Repl: '~'),
  (Html: 'part'; Text: #$B6; IsSym: true; Repl: 'd'),
  (Html: 'bull'; Text: #$B7; IsSym: true; Repl: '*'),
  (Html: 'ne'; Text: #$B9; IsSym: true; Repl: '=/='),
  (Html: 'equiv'; Text: #$BA; IsSym: true; Repl: '=='),
  (Html: 'asymp'; Text: #$BB; IsSym: true; Repl: '~'),
  (Html: 'otimes'; Text: #$C4; IsSym: true; Repl: 'x'), // not supported in IE6!
  (Html: 'oplus'; Text: #$C5; IsSym: true; Repl: '+'),
  (Html: 'empty'; Text: #$C6; IsSym: true),  // not supported in IE6!
  (Html: 'cap'; Text: #$C7; IsSym: true; Repl: 'n'),
  (Html: 'cup'; Text: #$C8; IsSym: true; Repl: 'U'),
  (Html: 'sup'; Text: #$C9; IsSym: true),
  (Html: 'supe'; Text: #$CA; IsSym: true),
  (Html: 'nsub'; Text: #$CB; IsSym: true),   // not supported in IE6!
  (Html: 'sub'; Text: #$CC; IsSym: true),
  (Html: 'sube'; Text: #$CD; IsSym: true),
  (Html: 'isin'; Text: #$CE; IsSym: true),
  (Html: 'notin'; Text: #$CF; IsSym: true),  // not supported in IE6!
  (Html: 'ang'; Text: #$D0; IsSym: true; Repl: '/|'),
  (Html: 'nabla'; Text: #$D1; IsSym: true; Repl: 'V'),
  (Html: 'reg'; Text: #$D2; IsSym: true; Repl: '(R)'),
  (Html: 'copy'; Text: #$D3; IsSym: true; Repl: '(C)'),
  (Html: 'trade'; Text: #$D4; IsSym: true; Repl: 'tm'),
  (Html: 'radic'; Text: #$D6; IsSym: true; Repl: '\|'),
  (Html: 'and'; Text: #$D9; IsSym: true; Repl: '&&'),
  (Html: 'or'; Text: #$DA; IsSym: true; Repl: '||'),
  (Html: 'hArr'; Text: #$DB; IsSym: true; Repl: '<=>'),
  (Html: 'lArr'; Text: #$DC; IsSym: true; Repl: '<='),  // not supported in IE6!
  (Html: 'uArr'; Text: #$DD; IsSym: true),  // not supported in IE6!
  (Html: 'rArr'; Text: #$DE; IsSym: true; Repl: '=>'),
  (Html: 'dArr'; Text: #$DF; IsSym: true),  // not supported in IE6!
  (Html: 'int'; Text: #$F2; IsSym: true; Repl: 'S')
  );
    
procedure HTMLTextOut(Canvas: TCanvas; const Txt: string; X, Y: integer; 
  Vertical: boolean);

procedure HTMLCalculateSize(Canvas: TCanvas; const Txt: string; var W, H: integer);

function HTML2Text(const Txt: string): string; 

type // used in ProcessLegendAction
  TProcessLegendAction=(plaNothing, plaRemove, plaReplace); 

function ProcessLegendAction(var Txt: string; Action: TProcessLegendAction; 
  ReplaceTxt: string): string;

implementation

// this procedure performs HTML output using temporary THTMLTextPainter
procedure HTMLTextOut(Canvas: TCanvas; const Txt: string; X, Y: integer; 
  Vertical: boolean);
begin
  with THTMLTextPainter.Create(Canvas, Txt, X, Y, Vertical) do
  try
    DrawText;
  finally
    Free;
  end;
end;

// only calculates width and height w/o drawing (depends on Vertical!)
procedure HTMLCalculateSize(Canvas: TCanvas; const Txt: string; var W, H: integer);
begin
  with THTMLTextPainter.Create(Canvas, Txt, 0, 0, false) do
  try
    CalculateSize;
    W:=Width;
    H:=Height;
  finally
    Free;
  end;
end;

// strips any HTML formatting and CRLFs
function HTML2Text(const Txt: string): string;
var
  I: integer;  
begin
  // remove tags
  Result:=StringReplace(Txt, #13#10, ' ', [rfReplaceAll]);
  Result:=StringReplace(Result, '<BR>', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '<B>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '</B>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '<I>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '</I>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '<U>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '</U>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '<SUB>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '</SUB>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '<SUP>', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '</SUP>', '', [rfReplaceAll, rfIgnoreCase]);
  // remove pseudotag
  ProcessLegendAction(Result, plaRemove, '');
  // replace entities
  for I:=0 to High(Entities) do
  begin
    if Entities[I].Repl<>''
    then Result:=StringReplace(Result, '&'+Entities[I].Html+';', 
        Entities[I].Repl, [rfReplaceAll])
    else Result:=StringReplace(Result, '&'+Entities[I].Html+';', 
      Entities[I].Text, [rfReplaceAll]);
  end; 
end;

// process <legend> pseudotag used in custom plot legends
function ProcessLegendAction(var Txt: string; Action: TProcessLegendAction; 
  ReplaceTxt: string): string;
var
  S: string;
  p1, p2, pp1: integer;
begin
  Result:=''; // default
  S:=LowerCase(Txt);
  p1:=Pos('<legend>', S);
  p2:=Pos('</legend>', S);
  pp1:=p1+Length('<legend>');
  if (p1>0) and (p2>0) and (p2>p1) then // legend exists!
  begin
    Result:=Copy(Txt, pp1, p2-pp1);
    if Action=plaRemove
    then Delete(Txt, p1, p2-p1+Length('</legend>'));
    if Action=plaReplace then
    begin
      Delete(Txt, p1, p2-p1+Length('</legend>'));
      Insert('<legend>'+ReplaceTxt+'</legend>', Txt, p1);
    end;
  end else // no legend - add!
    if Action=plaReplace 
    then Txt:=Txt+'<legend>'+ReplaceTxt+'</legend>';
end;
  
{---------- Class: THTMLTextPainter ----------} 

//: only calculates Width and Height (drawing is blocked)
procedure THTMLTextPainter.CalculateSize;
begin
  FCalcSizeMode:=true;
  try
    DrawText;
  finally
    FCalcSizeMode:=false;
  end;  
end;

//: process HTML named entities (such as &gt;)
procedure THTMLTextPainter.CheckEntities;
var
  I: integer;  
  EntLen: integer;
begin
  if (FCharCounter>FTextLength) or (FText[FCharCounter]<>'&')
  then Exit; // no entity in current position!
  for I:=0 to High(Entities) do
  begin
    EntLen:=Length(Entities[I].Html);
    if (FCharCounter+EntLen+1<=FTextLength) and
      (Copy(FText, FCharCounter+1, EntLen+1{;!})=Entities[I].Html+';') then
    begin
      if Entities[I].IsSym then
      begin
        FCanvas.Font.Name:='Symbol';
        UpdateFont;
        {if FVertical
        then FShift:=-(FSubSupShift div 2);}
      end;  
      DrawChar(Entities[I].Text);
      if Entities[I].IsSym then
      begin
        FCanvas.Font.Name:=FInitialFontName;
        FCanvas.Font.Charset:=DEFAULT_CHARSET; // <- win9x symbol bugfix
        UpdateFont;
        {if FVertical
        then FShift:=0;}
      end;  
      FCharCounter:=FCharCounter+EntLen+2; // move position beyond the entity
    end;
  end; 
end;

//: process all HTML formatting (called from DrawChars)
procedure THTMLTextPainter.CheckFormat;
begin
  CheckEntities; // draw named entities using DrawChar
  CheckTags; // process tags
  // check CRLF
  if (FCharCounter<FTextLength) and (FText[FCharCounter]=#13) // CRLF
    and (FText[FCharCounter+1]=#10) then
  begin
    Inc(FCharCounter, 2);
    NewLine;
    Exit;
  end;
end;

//: invoked only from CheckTags; returns true if tag detected
function THTMLTextPainter.CheckTag(const Tag: string): boolean;
var
  TagLen: integer;
begin
  Result:=false; // default
  TagLen:=Length(Tag);
  if FCharCounter+TagLen+1>FTextLength
  then Exit;
  Result:=UpperCase(Tag+'>')=Copy(FUText, FCharCounter+1, TagLen+1);
  if Result         // ^ discriminate <B> and <BR>!
  then FCharCounter:=FCharCounter+TagLen+2; // move beyond the tag 
end;

//: process tags - no drawing, only change font properties
procedure THTMLTextPainter.CheckTags;
begin
  if (FCharCounter>FTextLength) or (FText[FCharCounter]<>'<')
  then Exit; // no tag in current position!
  if CheckTag('B') then 
  begin
    FCanvas.Font.Style:=FCanvas.Font.Style+[fsBold];
    UpdateFont;
  end;
  if CheckTag('/B') then 
  begin
    FCanvas.Font.Style:=FCanvas.Font.Style-[fsBold];
    UpdateFont;
  end;
  if CheckTag('I') then 
  begin
    FCanvas.Font.Style:=FCanvas.Font.Style+[fsItalic];
    UpdateFont;
  end;
  if CheckTag('/I') then 
  begin
    FCanvas.Font.Style:=FCanvas.Font.Style-[fsItalic];
    UpdateFont;
  end;
  if CheckTag('U') then 
  begin
    FCanvas.Font.Style:=FCanvas.Font.Style+[fsUnderline];
    UpdateFont;
  end;
  if CheckTag('/U') then 
  begin
    FCanvas.Font.Style:=FCanvas.Font.Style-[fsUnderline];
    UpdateFont;
  end;
  if CheckTag('SUB') then 
  begin
    FShift:=FSubShift;
    FCanvas.Font.Size:=FSubSupFontSize;
    UpdateFont;
  end;
  if CheckTag('/SUB') then 
  begin
    FShift:=0;
    FCanvas.Font.Size:=FInitialFontSize;
    UpdateFont;
  end;
  if CheckTag('SUP') then 
  begin
    FShift:=FSupShift;
    FCanvas.Font.Size:=FSubSupFontSize;
    UpdateFont;
  end;
  if CheckTag('/SUP') then 
  begin
    FShift:=0;
    FCanvas.Font.Size:=FInitialFontSize;
    UpdateFont;
  end;
  if CheckTag('BR') 
  then NewLine;
  // todo : process HR??! other tags?
end;

//: initialize fields
constructor THTMLTextPainter.Create(C: TCanvas; const T: string; X,
  Y: integer; V: boolean);
begin
  inherited Create;
  // canvas must be valid!
  Assert(C is TCanvas, '{B88FE928-7CE4-48BB-981B-2294E936420F}');
  // init parameter buffers
  FCanvas:=C;
  FText:=T;
  ProcessLegendAction(FText, plaRemove, ''); // not used in output 
  FX:=X;
  FY:=Y;
  FVertical:=V;
  // init additional drawing vars
  FTextLength:=Length(FText);
  FUText:=UpperCase(T);
  FCharCounter:=1;
  FNewLineShift:=FCanvas.TextHeight('H'); // todo : is this generally wrong!?
  FSubShift:=Round(FNewLineShift*0.3);
  FSupShift:=-Round(FNewLineShift*0.2);
  FInitialFontSize:=FCanvas.Font.Size;
  FSubSupFontSize:=Round(FInitialFontSize*0.8);
  FLeft:=X;
  FTop:=Y+FSubShift div 2; //?
  FShift:=0;
  FCanvas.Brush.Style:=bsClear; // transparent background
  FInitialFontName:=FCanvas.Font.Name;
  FWidth:=0;
  FHeight:=FNewLineShift+FSubShift; //?  Round(FNewLineShift*1.1);
  FCalcSizeMode:=false;
end;

//: draw single character (called from DrawChars and CheckEntities)
procedure THTMLTextPainter.DrawChar(C: char);
begin
  if FVertical then
  begin
    if not FCalcSizeMode
    then FCanvas.TextOut(FLeft+FShift, FTop, C);
    FTop:=FTop-FCanvas.TextWidth(C);
    FWidth:=Max(FWidth, FY-FTop);
  end else
  begin
    if not FCalcSizeMode
    then FCanvas.TextOut(FLeft, FTop+FShift, C);
    FLeft:=FLeft+FCanvas.TextWidth(C);
    FWidth:=Max(FWidth, FLeft-FX);
  end;
end;

//: main character drawing cycle
procedure THTMLTextPainter.DrawText;
var
  Tmp: integer;
begin
  UpdateFont;
  FCharCounter:=1;
  while FCharCounter<=FTextLength do
  begin
    Tmp:=FCharCounter;
    repeat // process ALL formatting in current position (including adjacent)
      if Tmp<>FCharCounter
      then Tmp:=FCharCounter;
      CheckFormat;
    until Tmp=FCharCounter; 
    if FCharCounter>FTextLength // FCharCounter moved by CheckFormat!
    then Exit; // no more chars to draw!
    DrawChar(FText[FCharCounter]);
    Inc(FCharCounter);
  end;
end;

//: start new line (CRLF or <br>)
procedure THTMLTextPainter.NewLine;
begin
  if FVertical then
  begin
    FLeft:=FLeft+FNewLineShift;
    FTop:=FY;
  end else
  begin
    FTop:=FTop+FNewLineShift;
    FLeft:=FX;
  end;
  FHeight:=FHeight+FNewLineShift;
end;

//: recreates vertical font
procedure THTMLTextPainter.UpdateFont;
var
  LogFont: TLogFont;
begin
  if not FVertical
  then Exit; // no need to update!
  Assert(GetObject(FCanvas.Font.Handle, SizeOf(TLogFont), @LogFont)<>0,
    '{7E8E2EB4-1063-483E-8A13-84F7E2FFAED6}');
  LogFont.lfEscapement:=900; // 90 degrees * 10
  FCanvas.Font.Handle:=CreateFontIndirect(LogFont);
end;

end.
 