unit DirDrawU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, EanKod, Printers, ComCtrls, Registry;

type
  TEanDirectDraw = class(TForm)
    E_POSITION: TEdit;
    Label1: TLabel;
    EAN: TEan;
    E_WIDTH: TEdit;
    E_HEIGHT: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    E_LEFT: TEdit;
    E_TOP: TEdit;
    E_HOR: TEdit;
    E_VER: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    E_COUNT: TEdit;
    Label10: TLabel;
    PS: TPrinterSetupDialog;
    Label2: TLabel;
    PB: TProgressBar;
    BitBtn4: TBitBtn;
    Label9: TLabel;
    BitBtn5: TBitBtn;
    DemoLabel: TLabel;
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Demo : Boolean;
  public
    { Public declarations }
  end;

var
  EanDirectDraw: TEanDirectDraw;

implementation

{$R *.DFM}

procedure TEanDirectDraw.BitBtn2Click(Sender: TObject);
begin
        PS.Execute;
end;

procedure TEanDirectDraw.BitBtn1Click(Sender: TObject);
const inch = 25.4;
var Count    : Integer;

    // first position for print
    Position : Integer;
    LeftMargin,TopMargin,Horz,Vert,
    LabelWidth, LabelHeight : Double;

    // DPI in X and Y direction
    DPI_X, DPI_Y                    : integer;

    // physical paper size
    PageWidth, PageHeight           : Double;

    // non-printable margins
    OffsetLeft, OffsetTop           : Double;

    // Numbers of labels in row & column
    LabelRows, LabelCols            : Integer;

    // some working variables
    pt                              : TPoint;
    mm                              : Double;
    R                               : TRect;
    X,Y                             : Integer;

begin
   try
    Position   := StrToInt(E_Position.Text);
    Count      := StrToInt(E_Count.Text);
    LeftMargin := StrToFloat(E_Top.Text);
    TopMargin  := StrToFloat(E_Left.Text);
    Horz       := StrToFloat(E_Hor.Text);
    Vert       := StrToFloat(E_Ver.Text);
    LabelWidth := StrToFloat(E_Width.Text);
    LabelHeight:= StrToFloat(E_Height.Text);
   except
       raise;
   end;

   if Count<=0 then Exit;


   // here we read paper parameters ...
   DPI_X:=GetDeviceCaps(PRINTER.Handle,LOGPIXELSX);
   DPI_Y:=GetDeviceCaps(PRINTER.Handle,LOGPIXELSY);

   Escape(Printer.Handle,GETPHYSPAGESIZE,0,nil,@pt);
   PageWidth  := inch*(pt.X/DPI_X);
   PageHeight := inch*(pt.Y/DPI_Y);

   Escape(Printer.Handle,GETPRINTINGOFFSET,0,nil,@pt);
   OffsetLeft := inch*(pt.X/DPI_X);
   OffsetTop  := inch*(pt.Y/DPI_Y);

   LabelRows := 0;
   LabelCols := 0;

   mm:=LeftMargin;
   while (mm+LabelWidth+Horz < PageWidth) do begin
         Inc(LabelCols);
         mm:=mm+LabelWidth+Horz;
   end;

   mm:=TopMargin;
   while (mm+LabelHeight+Vert < PageHeight) do begin
         Inc(LabelRows);
         mm:=mm+LabelHeight+Vert;
   end;


   {ShowMessage(Format('DPI       :  %d/%d'+#13#10+
                      'Page      : %5.2f/%5.2f mm'+#13#10+
                      'Offset    : %5.2f/%5.2f mm'+#13#10+
                      'Rows/Cols : %d/%d',
                      [DPI_X,DPI_Y,PageWidth,PageHeight,OffsetLeft,OffsetTop,LabelRows,LabelCols]));

   }

   Printer.BeginDoc;
   try
      Screen.Cursor := crHourGlass;
      PB.Visible    := True;
      PB.Max        := Count;

      Printer.Title := Application.Title;
      Application.ProcessMessages;

      while Count>0 do begin
            if Position>=(LabelCols*LabelRows) then begin
               Printer.NewPage;
               Position := 0;
            end;

            // column & row for selected position
            X := Position mod LabelCols;
            Y := Position div LabelCols;

            // calc printable rectangle on Printer.Canvas in Pixels

            R.Left   := Round(((X*(LabelWidth+Horz))  - OffsetLeft + LeftMargin)*DPI_X/inch);
            R.Top    := Round(((Y*(LabelHeight+Vert)) - OffsetTop  + TopMargin)*DPI_Y/inch);
            R.Right  := Round(R.Left + LabelWidth *DPI_X/inch);
            R.Bottom := Round(R.Top  + LabelHeight*DPI_Y/inch);

            EAN.Print(R);

            if demo and (count mod 3 = 0) then begin
               Printer.Canvas.Pen.Width := 20;
               Printer.Canvas.Pen.Style := psSolid;
               Printer.Canvas.Pen.Color := clBlack;
               Printer.Canvas.MoveTo(R.Left,R.Top);
               Printer.Canvas.LineTo(R.Right,R.Bottom);
            end;

            Inc(Position);
            Dec(Count);

            PB.Position := PB.Max - Count;
      end;
   finally
      Screen.Cursor := crDefault;
      PB.Visible    := False;
      Printer.EndDoc;
   end;

end;

procedure TEanDirectDraw.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     MessageDlg( 'Thank you for using PSOFT Barcode printer.'+#13#10+
                 'If you need technical support, please mail to us.'+#13#10#13#10+
                 'EMail : peter@psoft.sk'+#13#10+
                 'WEB   : http://www.psoft.sk',
                 mtInformation,
                 [mbOK],0);
end;

procedure TEanDirectDraw.BitBtn4Click(Sender: TObject);
begin
     EAN.ActiveSetupWindow;
end;

procedure TEanDirectDraw.FormCreate(Sender: TObject);
// const SpravneSerialNo='670908-6450';
var reg      : TRegistry;
    SerialNo : String;
    KeyPath  : String;
begin
     Demo := False;
{     Application.ShowHint := True;
     reg      := TRegistry.Create;
     KeyPath  := 'SOFTWARE\PSOFT\barcode Printer';

     demo := True;
     try
        reg.RootKey := HKEY_LOCAL_MACHINE;
        if reg.OpenKey(KeyPath,False) then begin
           if Reg.ValueExists('Serial')  then SerialNo := Trim(reg.ReadString('Serial'));
           if SerialNo=SpravneSerialNo then Demo:=False;
        end;

        if Demo then
           if MessageDlg('Hello.'+#13#10+
                         'This is shareware version of PSOFT Barcode printer.'+#13#10+
                         'If you can register it, please read file README.TXT'+#13#10+#13#10+
                         'Have you your serial number ?',
                         mtConfirmation	,[mbYes,mbNo],0)=mrYes then
               begin
                    SerialNo := InputBox('If you register, type your serial number','Serial number','');
                    if (SerialNo='') or (SerialNo<>SpravneSerialNo) then
                       MessageDlg('Bad serial number, sorry ...',mtWarning,[mbOK],0)
                    else begin
                       reg.OpenKey(KeyPath,True);
                       reg.WriteString('Serial', SerialNo);
                       Demo:=False;
                    end;
               end;
     finally
        Reg.Free;
     end;
}
     DemoLabel.Visible := True;
end;

end.
