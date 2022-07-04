// unit About
//
// This unit takes charge of providing user with basic information on BASSPlay.
//
//       written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// Ver 1.1                        20 Apr 2009
//   - Modified for Delphi 2009
//
// Ver 1.0                        28 Jan 2009
//   - Initial release
//

unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    MessagePosY : integer;
    PassedCounter : DWORD;
    ClosedAtTop : boolean;

    procedure DrawAboutBox;

  public
    { Public declarations }

    StalledPeriod : boolean;
    ShowingAbout : boolean;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure StartAbout;
  end;

var
  AboutForm: TAboutForm;
  BASSPlayVer : string;
  TBASSPlayerVer : string;
  BASSLibVer : string;

implementation

{$R *.DFM}

const
   SAboutText: string =
     'BASSPlay is a Demo program to show you'#13#10 +
     ' the usage of TBASSPlayer.'#13#10 +
     'TBASSPlayer is a Delphi component based on'#13#10 +
     '  BASS Audio Library.'+ #13#10 +
     'Visit http://www.un4seen.com/ if you want to know'+ #13#10 +
     ' more about BASS audio library.'+ #13#10 +
     'The author of BASSPlay and TBASSPlayer : Silhwan Hyun'#13#10 + #13#10 +
     'Version Information'#13#10;

   SContributors : string =
     'Contributors'#13#10 + #13#10 + 'Emil Weiss';

   SAboutText_Han: string =
     'BASSPlay는 TBASSPlayer의 활용법을 보여주기 위한'#13#10 +
     ' 데모 프로그램입니다.'#13#10 +
     'TBASSPlayer는 BASS 오디오 라이브러리를 기반으로'#13#10 +
     ' 만들어진 델파이용 컴포넌트입니다.'#13#10 +
     'BASS 오디오 라이브러리에 대해 더 자세히 알고 싶으면'#13#10 +
     ' http://www.un4seen.com/ 을 방문해 보십시요.'#13#10 + #13#10 +
     'BASSPlay와 TBASSPlayer의 작성자 : 현실환'#13#10 + #13#10 +
     '버전 정보'#13#10;

   SContributors_Han: string =
     '도움 주신 분'#13#10 + #13#10 + 'Emil Weiss';

   WaitingCycle = 700;

var
   AboutMsg, AboutMsg_Han : string;
   VerStr1, VerStr2, VerStr3 : string;

procedure TAboutForm.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);

   Params.ExStyle := WS_EX_TOOLWINDOW;
end;

procedure TAboutForm.DrawAboutBox;
Var
   R, R2 : TRect;         
   TimeLeft : DWORD;
   WaitingText : string;
begin
   if not StalledPeriod then
     with Image1.Canvas do
     begin
       Brush.Color := clBlack;
       Font.Color  := $00CEFFCE;
     //  Font.Style  := [fsBold];
       Font.Style  := [];
       Font.Height := 18;
       FillRect(ClipRect);
       SetRect(R, 0, 0, Image1.Width, 0);

   // Calculate the size of rectangle region for drawing formatted text
       if GetOEMCP = 949 then   // OEM code page is Korean Language ?
          DrawText(Handle, pChar(AboutMsg_Han), -1,
                     R, DT_CALCRECT or DT_WORDBREAK or DT_CENTER or DT_NOPREFIX)
       else  
          DrawText(Handle, pChar(AboutMsg), -1,
                     R, DT_CALCRECT or DT_WORDBREAK or DT_CENTER or DT_NOPREFIX);
   // Adjust the coordination to put the text at desired position
       R2.Top    := MessagePosY;
       R2.Left   := Round((Image1.Width - (R.Right - R.Left)) / 2);
       R2.Right  := R2.Left + R.Right;
       R2.Bottom := R2.Top + R.Bottom;
   // Draw formatted text
       if GetOEMCP = 949 then   // OEM code page is Korean Language ?
          DrawText(Handle, pChar(AboutMsg_Han), -1,
                      R2, DT_WORDBREAK or DT_CENTER or DT_NOPREFIX)
       else
          DrawText(Handle, pChar(AboutMsg), -1,
                      R2, DT_WORDBREAK or DT_CENTER or DT_NOPREFIX);
     end
   else  // now stalled period
     with Image1.Canvas do
     begin
       TimeLeft := ((WaitingCycle - PassedCounter) * Timer1.Interval) div 1000 + 1;
       if GetOEMCP = 949 then   // OEM code page is Korean Language ?
          WaitingText := ' 이 윈도우는 ' + intToStr(TimeLeft) + ' 초후에 자동으로 닫힙니다.'
       else
          WaitingText := ' This window will be closed in ' + intToStr(TimeLeft) + ' seconds';
       Brush.Color := clBlack;
       Font.Color  := clGreen;
    //   Font.Style  := [fsBold];
       Font.Style  := [];
       Font.Height := 18;
       SetRect(R, 0, 0, Image1.Width, 0);

   // Calculate the size of rectangle region for drawing formatted text
       DrawText(Handle, pChar(WaitingText), -1, R,
                DT_CALCRECT or DT_WORDBREAK or DT_CENTER or DT_NOPREFIX);
   // Adjust the coordination to put the text at Bottom area
       R2.Top    := Image1.Height - (R.Bottom - R.Top) - 5;
       R2.Left   := Round((Image1.Width - (R.Right - R.Left)) / 2);
       R2.Right  := R2.Left + R.Right;
       R2.Bottom := R2.Top + R.Bottom;
   // Draw formatted text
       DrawText(Handle, pChar(WaitingText), -1, R2, DT_WORDBREAK or DT_CENTER or DT_NOPREFIX);
       Image1.Refresh;
     end;

   inc(PassedCounter);

   if ((MessagePosY + (R.Bottom - R.Top)) <= 0) then  // reched the top of display area ?
   begin
      ClosedAtTop := true;
      Self.Close;
      exit;
   end;

   if MessagePosY > (Image1.Height div 3) then
      Dec(MessagePosY, 3)
   else if MessagePosY > (Image1.Height div 6) then
      Dec(MessagePosY, 2)
   else if MessagePosY > 10 then
      Dec(MessagePosY, 1)
   else if PassedCounter > WaitingCycle then   // Wait for a while
   begin
      Dec(MessagePosY, 6);
      StalledPeriod := false;
   end else
      StalledPeriod := true;
end;

procedure TAboutForm.StartAbout;
begin
   MessagePosY := Image1.Height; // Set initial vertical position to bottom of display area
   StalledPeriod := false;
   PassedCounter := 0;
   ShowingAbout := true;
   ClosedAtTop := false;

   VerStr1 := 'BASSPlay : ' + BASSPlayVer + #13#10;
   VerStr2 := 'TBASSPlayer : ' + TBASSPlayerVer + #13#10;
   if GetOEMCP = 949 then
   begin
      Self.Caption := 'BASSPlay는..';
      VerStr3 := 'BASS 오디오 라이브러리 : ' + BASSLibVer + #13#10 + #13#10
   end else
      VerStr3 := 'BASS audio library : ' + BASSLibVer + #13#10 + #13#10;
   AboutMsg := SAboutText + VerStr1 + VerStr2 + VerStr3 + SContributors;
   AboutMsg_Han := SAboutText_Han + VerStr1 + VerStr2 + VerStr3 + SContributors_Han;

   Self.Show;
   Timer1.Enabled := true;
end;

procedure TAboutForm.Timer1Timer(Sender: TObject);
begin
   DrawAboutBox;
end;

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   ShowingAbout := False;
   Timer1.Enabled := false;

   if not ClosedAtTop then   // Clear Image for next showing (Previous image may be shown)
     with Image1.Canvas do
     begin
       Brush.Color := clBlack;
       FillRect(ClipRect);
       Image1.Refresh;
     end;
end;

end.
