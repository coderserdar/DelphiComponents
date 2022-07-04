program SoftPhone;

uses
  Forms,
  PhoneSettings in 'PhoneSettings.pas' {frmPhoneSettings},
  NokiaStyle in 'NokiaStyle.pas' {frmNokiaStyle},
  EditContact in 'EditContact.pas' {frmEditContact},
  About in 'About.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNokiaStyle, frmNokiaStyle);
  Application.CreateForm(TfrmPhoneSettings, frmPhoneSettings);
  Application.CreateForm(TfrmEditContact, frmEditContact);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
