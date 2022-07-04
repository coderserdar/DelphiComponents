unit ipsFormPrinter;

interface

uses Classes, Windows, Forms, StdCtrls, SysUtils,
        iPaperParams;

type
        TpsFePrintPageEvent = procedure (Sender:TObject; Current, Total:Integer) of Object;
        TpsFePrintLabelEvent = procedure (Sender:TObject; Current, Total:Integer; R:TRect) of Object;

        TpsFormPrinter= class(TComponent)
        private
                FShowProgress: Boolean;
                FOnBeforePrint: TNotifyEvent;
                FOnAfterPrint: TNotifyEvent;
                FOnAfterPrintLabel: TpsFePrintLabelEvent;
                FOnBeforePrintLabel: TpsFePrintLabelEvent;
                FOnBeforePrintPage: TpsFePrintPageEvent;
                FOnAfterPrintPage: TpsFePrintPageEvent;
                FPrintParams: TpsPrintParams;
                FCountOne: Integer;
                FCount: Integer;
                FAbort: Boolean;
                function CreateProgressForm(F:TForm):TLabel;

        public
                constructor Create(AOwner:TComponent); override;
                destructor  Destroy; override;
                function  TotalPages : Integer;
                procedure Print;
                procedure AbortPrint;
                procedure PrintOneLabel(R:TRect);
        published
                property PrintParams:TpsPrintParams Read FPrintParams Write FPrintParams;
                property ShowProgress:Boolean Read FShowProgress Write FShowProgress;
                property Count:Integer Read FCount Write FCount;
                property CountOne:Integer Read FCountOne Write FCountOne;
                property OnBeforePrint:TNotifyEvent Read FOnBeforePrint Write FOnBeforePrint;
                property OnAfterPrint:TNotifyEvent Read FOnAfterPrint Write FOnAfterPrint;
                property OnBeforePrintLabel:TpsFePrintLabelEvent Read FOnBeforePrintLabel Write FOnBeforePrintLabel;
                property OnAfterPrintLabel:TpsFePrintLabelEvent  Read FOnAfterPrintLabel  Write FOnAfterPrintLabel;
                property OnAfterPrintPage:TpsFePrintPageEvent   Read FOnAfterPrintPage   Write FOnAfterPrintPage;
                property OnBeforePrintPage:TpsFePrintPageEvent   Read FOnBeforePrintPage   Write FOnBeforePrintPage;
        end;


implementation

uses Printers;
{ TpsFormPrinter }

procedure TpsFormPrinter.AbortPrint;
begin
        FAbort := True;
end;

constructor TpsFormPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintParams:= TpsPrintParams.CreateWithParams;
end;

function TpsFormPrinter.CreateProgressForm(F: TForm): TLabel;
begin
        Result := nil;
end;

destructor TpsFormPrinter.Destroy;
begin
  FPrintParams.Free;
  inherited;
end;

procedure TpsFormPrinter.Print;
var F:TForm;
    _lbl_count, _total_count : Integer;
    LBL        : TLabel;
    i,j        : Integer;
    R          : TRect;
begin
    F   := nil;
    LBL := nil;

    if ShowProgress then LBL:=CreateProgressForm(F);
    if Assigned(FOnBeforePrint) then
        FOnBeforePrint(Self);

    if FCountOne<=0 then FCountOne :=1;
    _lbl_count   := 0;
    _total_count := Count;
    FAbort       := False;

    Printer.BeginDoc;
    try
        for i:=1 to FCount do begin
           for j:=1 to FCountOne do begin

                Inc(_lbl_count);

                if ShowProgress then begin
                        LBL.caption := Format('%d/%d',[_lbl_count, _total_count]);
                end;
                if Assigned(FOnAfterPrintLabel) then
                        FOnBeforePrintLabel(Self, _lbl_count, Count, R);

                PrintOneLabel(R);

                if Assigned(FOnAfterPrintlabel) then
                        FOnBeforePrintLabel(Self, _lbl_count, Count, R);
                if FAbort then Break;
           end;
           if FAbort then Break;
        end;
    finally
        if FAbort then Printer.Abort
        else           Printer.EndDoc;
    end;
    if Assigned(FOnAfterPrint) then
        FOnBeforePrint(Self);
end;

procedure TpsFormPrinter.PrintOneLabel(R: TRect);
begin
        {}
end;

function TpsFormPrinter.TotalPages: Integer;
begin
        {}
end;

end.
 