unit smLngCmpDsgn;
//***************************************************************************//
//                                                                             
//           File generated by Smartbears Language Tools version 0.95          
//           www.smartbears.com                                                
//           for support email lng_tlz@smartbears.com                          
//           for information about outsourcing activity                        
//           mail contactus@smartbears.com                                     
//                                                                             
//           Components registered:                                               
//           TsmAction                                                
//           TsmRichEdit                                                
//           TsmDBText                                                
//           TsmForm                                                
//           TsmLabel                                                
//            19.01.2003 4:15:39                                               
//                                                             
//***************************************************************************//

interface
uses Classes;

procedure Register;

implementation

uses TypInfo, DesignIntf, smLanguage, smLngCmp, smPersistence, smLanguageProp;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TsmLanguage), TsmAction, 'Language', TsmLanguageProperty);
  RegisterPropertyEditor(TypeInfo(TsmLanguage), TsmRichEdit, 'Language', TsmLanguageProperty);
  RegisterPropertyEditor(TypeInfo(TsmLanguage), TsmDBText, 'Language', TsmLanguageProperty);
  RegisterPropertyEditor(TypeInfo(TsmLanguage), TsmForm, 'Language', TsmLanguageProperty);
  RegisterPropertyEditor(TypeInfo(TsmLanguage), TsmLabel, 'Language', TsmLanguageProperty);
end;

end.