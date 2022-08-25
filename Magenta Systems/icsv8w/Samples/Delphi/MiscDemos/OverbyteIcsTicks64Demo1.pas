{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  GetTickCount64 test routines for OverbyteIcsTicks64 unit.
Creation:     3 September 2009
Updated:      3 September 2009
Version:      1.00
Email:        delphi@magsys.co.uk  http://www.magsys.co.uk
Legal issues: Copyright (C) 2009 by Magenta Systems Ltd, England

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

Updates:


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit OverbyteIcsTicks64Demo1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, TypInfo,
  OverbyteIcsTicks64;

type
  TForm1 = class(TForm)
    LabelInfo: TLabel;
    doUpdate: TButton;
    doExit: TButton;
    TickModes: TListBox;
    Timer1: TTimer;
    Label1: TLabel;
    procedure doUpdateClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TickModesClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  StartTick: int64 ;

implementation

{$R *.dfm}

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

procedure TForm1.doUpdateClick(Sender: TObject);

    function SepMs (S: string): string ;
    begin
        result := Copy (S, 1, Length (S) - 3) + '-' + Copy (S, Length (S) - 2, 3) ;
    end;

begin
   LabelInfo.Caption :=
         'IcsGetTickCount64 = ' + SepMs (IntToStr (IcsGetTickCount64))  + #13+#10 +
         'Method = ' + TypInfo.GetEnumName (TypeInfo (TTicks64Mode), Ord (Ticks64Mode)) + #13+#10 +
         'GetTickCount = ' + SepMs (IntToStr (GetTickCount)) + #13+#10 +
         'PerfCountsPerSec = ' + IntToStr (IcsGetPerfCountsPerSec) + #13+#10 +
         'IcsPerfCountCurrMilli = ' + SepMs (IntToStr (IcsPerfCountCurrMilli)) + #13+#10 +
         'IcsNowPC = ' + FormatDateTime (ISODateLongTimeMask, IcsNowPC) + #13+#10 +
         'IcsLastBootDT = ' + FormatDateTime (ISODateTimeMask, IcsLastBootDT) + #13+#10 +
         'ProgRunSecs = ' + IntToStr (IcsElapsedSecs64 (StartTick)) + #13+#10 +
         'MaxPerfDays = ' + IntToStr (High (int64) div IcsGetPerfCountsPerSec div SecsPerDay) ; 
end;

procedure TForm1.FormShow(Sender: TObject);
begin
    StartTick := IcsGetTickCount64;
    doUpdateClick(Self);
end;

procedure TForm1.TickModesClick(Sender: TObject);
begin
    IcsInitTick64 (TTicks64Mode (TickModes.ItemIndex + 1));
    doUpdateClick(Self);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
    doUpdateClick(Self);
end;

end.
