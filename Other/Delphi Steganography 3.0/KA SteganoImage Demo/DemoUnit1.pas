unit DemoUnit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls, ToolWin, ExtDlgs,
  Buttons, ImgList, KASteganographyImage;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenImage1: TMenuItem;
    SaveBitmapwithencrypteddata1: TMenuItem;
    N1: TMenuItem;
    EmbedDataFile1: TMenuItem;
    SaveDataFile1: TMenuItem;
    ToolButton4: TToolButton;
    ScrollBox1: TScrollBox;
    OpenPictureDialog1: TOpenPictureDialog;
    KASteganographyImage1: TKASteganographyImage;
    SavePictureDialog1: TSavePictureDialog;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Close1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Memo1: TMemo;
    Panel3: TPanel;
    BitBtn1: TBitBtn;
    Splitter1: TSplitter;
    Message1: TMenuItem;
    Embedintheimage1: TMenuItem;
    Panel4: TPanel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    Label3: TLabel;
    PPanel: TPanel;
    Panel5: TPanel;
    Image1: TImage;
    CheckBox1: TCheckBox;
    Panel6: TPanel;
    Memo2: TMemo;
    CheckBox2: TCheckBox;
    N3: TMenuItem;
    procedure OpenImage1Click(Sender: TObject);
    procedure SaveBitmapwithencrypteddata1Click(Sender: TObject);
    procedure EmbedDataFile1Click(Sender: TObject);
    procedure SaveDataFile1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure KASteganographyImage1Progress(Sender: TObject; Current,
      Total: Integer);
    procedure FormResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure KASteganographyImage1ImageProcessingDone(Sender: TObject;
      MustSave: Boolean; var Abort: Boolean);
    procedure KASteganographyImage1NeedNextImage(Sender: TObject;
      ImageNumber: Integer; var Abort: Boolean);
  private
    { Private declarations }
    LastPassword : String;
    LastFileName : String;
    Cap          : String;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses DemoPasswordDialog, DemoProtectDialog, DemoNextImageDialog;

{$R *.dfm}

procedure TForm1.OpenImage1Click(Sender: TObject);
begin
 if OpenPictureDialog1.Execute Then
    Begin
      Caption                                  := Cap+' ['+ExtractFileName(OpenPictureDialog1.FileName)+']';
      SaveBitmapWithEncryptedData1.Enabled     := False;
      ToolButton2.Enabled                      := False;
      EmbedDataFile1.Enabled                   := False;
      ToolButton3.Enabled                      := False;
      SaveDataFile1.Enabled                    := False;
      ToolButton4.Enabled                      := False;
      Close1.Enabled                           := False;
      LastFileName                             := OpenPictureDialog1.FileName;
      Memo1.Lines.Text                         := '';
      KASteganographyImage1.Clear;
      KASteganographyImage1.LoadFromFile(OpenPictureDialog1.FileName);
      if KASteganographyImage1.HasEmbeddedData Then
         Begin
           if Not KASteganographyImage1.ValidPassword(LastPassword) Then
              Begin
                 Repeat
                   PasswordForm.Edit2.Text := '';
                   PasswordForm.ShowModal;
                 Until (PasswordForm.ModalResult=mrCancel) or (KASteganographyImage1.ValidPassword(Trim(PasswordForm.Edit2.Text)));
                 if NOT (PasswordForm.ModalResult=mrCancel) Then LastPassword := Trim(PasswordForm.Edit2.Text)
              End
           Else
              Begin
                PasswordForm.ModalResult := mrOk;
              End;
           if NOT (PasswordForm.ModalResult=mrCancel) Then
              Begin
                if KASteganographyImage1.ImageSequenceNo = 1 Then
                   Begin
                      SaveDataFile1.Enabled                := True;
                      ToolButton4.Enabled                  := True;
                      SaveBitmapWithEncryptedData1.Enabled := True;
                      ToolButton2.Enabled                  := True;
                      Close1.Enabled                       := True;
                      KASteganographyImage1.Password       := LastPassword;
                      PasswordForm.Edit2.Text              := '';
                      PPanel.Visible                       := True;
                      KASteganographyImage1.Enabled        := False;
                      ToolBar1.Enabled                     := False;
                      Panel1.Enabled                       := False;
                      File1.Enabled                        := False;
                      Message1.Enabled                     := False;
                      Try
                        KASteganographyImage1.ActionType   := atDecode;
                        KASteganographyImage1.Activate     := True;
                        if CheckBox1.Checked Then
                           Begin
                             Memo1.Lines.Text                := KASteganographyImage1.GetAsString;
                           End;
                      Finally
                        PPanel.Visible                     := False;
                        KASteganographyImage1.Enabled      := True;
                        ToolBar1.Enabled                   := True;
                        Panel1.Enabled                     := True;
                        File1.Enabled                      := True;
                        Message1.Enabled                   := True;
                      End;
                   End
                Else
                   Begin
                     MessageBox(Handle,'This image has sequence number > 1.'#13#10'Please open first image of the sequence!','Warning!',MB_ICONEXCLAMATION OR MB_OK);
                     KASteganographyImage1.Clear;
                     Exit;
                   End;
              End;
           BitBtn1.Enabled                           := True;
           EmbedInTheImage1.Enabled                  := True;
         End
      Else
         Begin
           EmbedDataFile1.Enabled              := True;
           ToolButton3.Enabled                 := True;
           BitBtn1.Enabled                     := True;
           EmbedInTheImage1.Enabled            := True;
           Close1.Enabled                      := True;
           Label2.Caption                      := 'Size: ';
         End;
    End;
end;

procedure TForm1.SaveBitmapWithEncryptedData1Click(Sender: TObject);
begin
  if KASteganographyImage1.HasEmbeddedData Then
     Begin
        if SavePictureDialog1.Execute Then
           Begin
             KASteganographyImage1.SaveToFile(SavePictureDialog1.FileName);
             Caption := Cap+' ['+ExtractFileName(SavePictureDialog1.FileName)+']';
           End;
     End;
end;

procedure TForm1.EmbedDataFile1Click(Sender: TObject);
Var
  FS : TFileStream;
begin
  if KASteganographyImage1.HasEmbeddedData Then
      Begin
        if MessageBox(Handle,'This image already has embedded data!'#13#10'Do you want to overwrite the embedded data in the image?','Warning!',MB_ICONQUESTION or MB_YESNOCANCEL)<>ID_YES Then Exit;
      End;
  if OpenDialog1.Execute Then
     Begin
       ProtectForm.Edit1.Text := '';
       ProtectForm.Edit2.Text := '';
       Repeat
         ProtectForm.ShowModal;
       Until (ProtectForm.ModalResult=mrCancel) or ((Trim(ProtectForm.Edit1.Text)=Trim(ProtectForm.Edit2.Text)) And (Trim(ProtectForm.Edit1.Text) <> ''));
       if Not (ProtectForm.ModalResult=mrCancel) Then
          Begin
            LastPassword                   := Trim(ProtectForm.Edit1.Text);
            FS                             := TFileStream.Create(OpenDialog1.FileName,fmOpenRead or fmShareDenyNone);
            PPanel.Visible                 := True;
            KASteganographyImage1.Enabled  := False;
            ToolBar1.Enabled               := False;
            Panel1.Enabled                 := False;
            File1.Enabled                  := False;
            Message1.Enabled               := False;
            Try
              KASteganographyImage1.Password       := ProtectForm.Edit1.Text;
              KASteganographyImage1.SetEmbeddedData(FS);
              KASteganographyImage1.ActionType     := atEncode;
              KASteganographyImage1.Activate       := True;
              SaveBitmapWithEncryptedData1.Enabled := True;
              ToolButton2.Enabled                  := True;
              if (CheckBox1.Checked) Then
                 Begin
                   Memo1.Lines.Text                := KASteganographyImage1.GetAsString;
                 End;
              SaveDataFile1.Enabled                := True;
              ToolButton4.Enabled                  := True;
            Finally
              FS.Free;
              PPanel.Visible                 := False;
              KASteganographyImage1.Enabled  := True;
              ToolBar1.Enabled               := True;
              Panel1.Enabled                 := True;
              File1.Enabled                  := True;
              Message1.Enabled               := True;
            End;
          End;
       ProtectForm.Edit1.Text := '';
       ProtectForm.Edit2.Text := '';
     End;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
   if Memo1.Lines.Count=0 Then
      Begin
        MessageBox(Handle,'There is no text data to embed!'#13#10'Please type some text in the "Embedded Message" memo.','Stop!',MB_ICONEXCLAMATION or MB_OK);
        Exit;
      End;
   if KASteganographyImage1.HasEmbeddedData Then
      Begin
        if MessageBox(Handle,'This image already has embedded data!'#13#10'Do you want to overwrite the embedded data in the image?','Warning!',MB_ICONQUESTION or MB_YESNOCANCEL)<>ID_YES Then Exit;
      End;
   ProtectForm.Edit1.Text := '';
   ProtectForm.Edit2.Text := '';
   Repeat
     ProtectForm.ShowModal;
   Until (ProtectForm.ModalResult=mrCancel) or ((Trim(ProtectForm.Edit1.Text)=Trim(ProtectForm.Edit2.Text)) And (Trim(ProtectForm.Edit1.Text) <> ''));
   if Not (ProtectForm.ModalResult=mrCancel) Then
      Begin
        KASteganographyImage1.Password       := ProtectForm.Edit1.Text;
        KASteganographyImage1.SetEmbeddedData(Memo1.Lines);
        KASteganographyImage1.ActionType     := atEncode;
        PPanel.Visible                       := True;
        KASteganographyImage1.Enabled        := False;
        ToolBar1.Enabled                     := False;
        Panel1.Enabled                       := False;
        File1.Enabled                        := False;
        Message1.Enabled                     := False;
        Try
          KASteganographyImage1.Activate     := True;
          SaveDataFile1.Enabled              := True;
          ToolButton4.Enabled                := True;
        Finally
          PPanel.Visible                     := False;
          KASteganographyImage1.Enabled      := True;
          ToolBar1.Enabled                   := True;
          Panel1.Enabled                     := True;
          File1.Enabled                      := True;
          Message1.Enabled                   := True;
        End;
        SaveBitmapWithEncryptedData1.Enabled := True;
        ToolButton2.Enabled                  := True;
      End;
end;

procedure TForm1.SaveDataFile1Click(Sender: TObject);
Var
  MS : TMemoryStream;
begin
  If SaveDialog1.Execute Then
     Begin
       MS := TMemoryStream.Create;
       Try
         MS.LoadFromStream(KASteganographyImage1.GetAsStream);
         MS.Position := 0;
         MS.SaveToFile(SaveDialog1.FileName);
       Finally
         MS.Free;
       End;
     End;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  KASteganographyImage1.Clear;
  Memo1.Lines.Text                         := '';
  PasswordForm.Edit2.Text                  := '';
  ProtectForm.Edit1.Text                   := '';
  ProtectForm.Edit2.Text                   := '';
  Label2.Caption                           := 'Size:';
  SaveBitmapWithEncryptedData1.Enabled     := False;
  ToolButton2.Enabled                      := False;
  EmbedDataFile1.Enabled                   := False;
  ToolButton3.Enabled                      := False;
  SaveDataFile1.Enabled                    := False;
  ToolButton4.Enabled                      := False;
  BitBtn1.Enabled                          := False;
  EmbedInTheImage1.Enabled                 := False;
  Close1.Enabled                           := False;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.KASteganographyImage1Progress(Sender: TObject; Current, Total: Integer);
Var
  Cur : Integer;
begin
  Cur                   := Round((Current/Total)*100);
  if Cur <> ProgressBar1.Position Then
     Begin
       ProgressBar1.Min      := 0;
       ProgressBar1.Max      := 100;
       ProgressBar1.Position := Cur;
       Application.ProcessMessages;
     End;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Panel4.Width := ToolBar1.Width-(ToolButton4.Left+ToolButton4.Width)+4;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  WinExec('Explorer.exe http://www.kadao.dir.bg/',SW_SHOWNORMAL);
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  Label2.Caption := 'Size: '+IntToStr(Length(Memo1.Lines.Text));
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if  (CheckBox1.Checked)
  And (KASteganographyImage1.HasEmbeddedData)
  And (KASteganographyImage1.ValidPassword(LastPassword)) Then
      Begin
        if NOT KASteganographyImage1.Decoded Then
           Begin
             KASteganographyImage1.ActionType   := atDecode;
             KASteganographyImage1.Activate     := True;
           End;
        Memo1.Lines.Text := KASteganographyImage1.GetAsString;
      End
  Else
  if (NOT (CheckBox1.Checked)) Then
      Begin
        Memo1.Lines.Text := '';
      End;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Cap := Caption;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked Then
     Begin
       Panel6.Visible := True;
     End
  Else
     Begin
       Panel6.Visible := False;
     End;
end;

procedure TForm1.KASteganographyImage1ImageProcessingDone(Sender: TObject; MustSave: Boolean; var Abort: Boolean);
Var
  Img : TKASteganographyImage;
  S   : String;
begin
  if Sender is TKASteganographyImage Then
     Begin
       Img := TKASteganographyImage(Sender);
       if (Img.ActionType=atEncode) And (Img.MultiEmbedding) And (Img.GetMaxEmbedSzie < Img.EmbeddedDataSize) Then
          Begin
            if MustSave Then
               Begin
                 S := SavePictureDialog1.Title;
                 SavePictureDialog1.Title    := S+' - sequence number: '+IntToStr(KASteganographyImage1.ImageSequenceNo);
                 if SavePictureDialog1.Execute Then
                    Begin
                      KASteganographyImage1.SaveToFile(SavePictureDialog1.FileName);
                      Caption := Cap+' ['+ExtractFileName(SavePictureDialog1.FileName)+']';
                    End
                 Else
                    Begin
                      Abort := True;
                    End;
                 SavePictureDialog1.Title := S;   
               End;
          End;
     End;
end;

procedure TForm1.KASteganographyImage1NeedNextImage(Sender: TObject; ImageNumber: Integer; var Abort : Boolean);
Var
  Img : TKASteganographyImage;
  S   : String;
begin
  if Sender is TKASteganographyImage Then
     Begin
       Repeat
           Img                         := TKASteganographyImage(Sender);
           S                           := NextImageForm.Caption;
           NextImageForm.Caption       := S+' - seq. number: '+IntToStr(KASteganographyImage1.ImageSequenceNo+1);
           OpenPictureDialog1.FileName := LastFileName;
           NextImageForm.Edit1.Text    := LastFileName;
           if Img.ActionType=atDecode Then
              Begin
                NextImageForm.Label2.Caption := 'containing part of the encrypted data.';
                NextImageForm.Dialog         := OpenPictureDialog1;
              End;
           if Img.ActionType=atEncode Then
              Begin
                NextImageForm.Label2.Caption  := 'to use as container to store part of the encrypted data.';
                NextImageForm.Dialog          := OpenPictureDialog1;
              End;
           Abort := NOT NextImageForm.Execute;
           if Not Abort Then
              Begin
                if FileExists(NextImageForm.FileName) Then
                   Begin
                     Caption := Cap+' ['+ExtractFileName(NextImageForm.FileName)+']';
                     Img.LoadFromFile(NextImageForm.FileName);
                     LastFileName := NextImageForm.FileName;
                     OpenPictureDialog1.FileName := LastFileName;
                     NextImageForm.Edit1.Text    := LastFileName;
                   End;
              End;
           NextImageForm.Caption := S;   
       Until (Abort) Or (FileExists(NextImageForm.FileName));
     End;
end;

end.
