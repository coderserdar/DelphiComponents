�
 TFORM1 0�  TPF0TForm1Form1Left� Top� Width�HeightwCaption<Tools&Comps - Security Components for Delphi and C++ BuilderColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	OnCreate
FormCreatePixelsPerInch`
TextHeight TButtonButton1LeftTopWidth� HeightCaptionNumber of Current UsersTabOrder OnClickButton1Click  TMemoMemo1Left� Top Width�Height\AlignalRightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Lines.StringsFThis sample application shows how you can restrict the number of users)running the application at the same time. LThe property MaxCurrentUsers indicates the maximum number of users that can be 5using the application at the same time. The property HaltOnReachMaxCurrentUsers Ntells the component to halt the application if the maximum number of users is 	reached.  OIf MaxCurrentUsers is 0 (Zero), there is no restriction to the number of users running !the application at the same time.  
ParentFontTabOrder  TButtonButton2LeftTop8Width� HeightCaptionUsers Admin.TabOrderOnClickButton2Click  TUsersCSUsersCS1	AutoLoginAppKeyMaxCurrentUsersMaxCurrentUsersHaltOnReachMaxCurrentUsers	Version1.90 (27/Dez)DatabaseNamedbUsersFileNames83DOSLeftvTop�   	TDatabasedbUsersDatabaseNamedbUsers
DriverNameSTANDARDParams.Strings*PATH=C:\ToolsAndComps\UsersCS\Samples\DataENABLE BCD=FALSEDEFAULT DRIVER=PARADOX SessionNameDefaultTransIsolationtiDirtyReadBeforeConnectdbUsersBeforeConnectLeft8Top   