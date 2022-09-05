// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  17597: uFormCombine2RGB.pas 
//
//    Rev 1.4    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.3    28-01-2006 18:42:10  mcm    Version: IMG 2.14
// Included an optional Alpha channel drop down listbox.
//
//   Rev 1.2    20-12-2004 22:58:08  mcm
// Modified to use TmcmInt

//
//   Rev 1.1    27-01-2003 13:49:22  mcm

//
//   Rev 1.0    27-05-2002 16:22:30  mcm

unit uFormCombine2RGB;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls;
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;
     {$ENDIF}
                     
type
  TFormCombineImage = class(TForm)
    btnOK           : TButton;
    btnCancel       : TButton;
    gbCombine       : TGroupBox;
    lChannel1       : TLabel;
    lChannel2       : TLabel;
    lChannel3       : TLabel;
    lChannel4       : TLabel;
    cbChannel1      : TComboBox;
    cbChannel2      : TComboBox;
    cbChannel3      : TComboBox;
    cbChannel4      : TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FNumChannels : word;
    function   GetSelectedImage(Index : word) : pointer;
    procedure  SetChannelNames(Index : word; Value : string);
    procedure  SetFormatName(Value : string);
    procedure  SetNumChannels(Value : word);
  public
    { Public declarations }
    property ChannelNames[Index : word] : string
      write  SetChannelNames;
    property FormatName : string
      write  SetFormatName;
    property NumChannels : word
      read   FNumChannels
      write  SetNumChannels;
    property SelectedImage[Index : word] : pointer
      read   GetSelectedImage;
  end;

var FormCombineImage : TFormCombineImage;

implementation

{$R *.DFM}

uses uChildWin;

procedure TFormCombineImage.FormCreate(Sender : TObject);
var i : integer;
begin
  if ((Owner as TForm).MDIChildCount > 0)
  then begin
       for i := ((Owner as TForm).MDIChildCount - 1) downto 0
       do begin
          if ((Owner as TForm).MDIChildren[i] is TFormChild)
          then begin
               cbChannel1.Items.AddObject((Owner as TForm).MDIChildren[i].Caption, (Owner as TForm).MDIChildren[i]);
               cbChannel2.Items.AddObject((Owner as TForm).MDIChildren[i].Caption, (Owner as TForm).MDIChildren[i]);
               cbChannel3.Items.AddObject((Owner as TForm).MDIChildren[i].Caption, (Owner as TForm).MDIChildren[i]);
               cbChannel4.Items.AddObject((Owner as TForm).MDIChildren[i].Caption, (Owner as TForm).MDIChildren[i]);
          end;
       end;
  end;
end; // TFormCombineImage.FormCreate.


procedure TFormCombineImage.SetChannelNames(Index : word; Value : string);
begin
  case Index of
  1 : lChannel1.Caption := Value + ':';
  2 : lChannel2.Caption := Value + ':';
  3 : lChannel3.Caption := Value + ':';
  4 : begin
        lChannel4.Caption := Value + ':';
        if (Value = 'Alpha')
        then begin
             cbChannel4.Items.InsertObject(0, ' ', Nil);
             cbChannel4.ItemIndex := 0;
        end;
      end;
  end;
end; // TFormCombineImage.SetChannelNames.


procedure TFormCombineImage.SetFormatName(Value : string);
begin
  gbCombine.Caption := 'Combine ' + Value + ' to RGB';
end; // TFormCombineImage.SetFormatName.


procedure TFormCombineImage.SetNumChannels(Value : word);
begin
  FNumChannels := Value;
  if (FNumChannels = 3)
  then begin
       lChannel4.Visible := False;
       cbChannel4.Visible := False;
       gbCombine.Height := 121; // On four channels = 153;
       btnOK.Top := 136; // On four channels = 168
       btnCancel.Top := 136; // On four channels = 168
       Height := 200; // On four channels = 227;
  end;
end; // TFormCombineImage.SetNumChannels.


function TFormCombineImage.GetSelectedImage(Index : word) : pointer;
begin
  case Index of
  1 : Result := cbChannel1.Items.Objects[cbChannel1.ItemIndex];
  2 : Result := cbChannel2.Items.Objects[cbChannel2.ItemIndex];
  3 : Result := cbChannel3.Items.Objects[cbChannel3.ItemIndex];
  4 : Result := cbChannel4.Items.Objects[cbChannel4.ItemIndex];
  else Result := Nil;
  end;
end; // TFormCombineImage.GetSelectedImage.

end.
