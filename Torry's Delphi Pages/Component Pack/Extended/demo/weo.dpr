program weo;


uses
  Forms,
  U_FormMainIni,
  U_XMLFenetrePrincipale,
  U_Splash,
  Windows,
  U_Donnees,
  U_CustomFrameWork,
  ADODB,
  fonctions_ObjetsXML,
  fonctions_string,
  Dialogs,
  U_ConstMessage in 'U_ConstMessage.pas';

{$R *.res}
{$R WindowsXP.res}


var
	gc_classname: Array[0..255] of char;
	gi_result: integer;

begin
	Application.Initialize;
	Application.Title := 'Test';

	// Met dans gc_classname le nom de la class de l'application
	GetClassName(Application.handle, gc_classname, 254);

  gs_NomApp := fs_GetNameSoft;
  
	// Renvoie le Handle de la première fenêtre de Class (type) gc_classname
	// et de titre TitreApplication (0 s'il n'y en a pas)
	gi_result := FindWindow(gc_classname, @gs_NomApp);

	if gi_result <> 0 then   // Une instance existante trouvée
		begin
			ShowWindow(gi_result, SW_RESTORE);
			SetForegroundWindow(gi_result);
			Application.Terminate;
			Exit;
		end
	else  // Première création
		begin
			Application.Title := gs_NomApp;
		end;

	F_SplashForm := TF_SplashForm.Create(Application);
	F_SplashForm.Label1.Caption := 'GENERIC' ;
	F_SplashForm.Label1.Width   := F_SplashForm.Width ;
	F_SplashForm.Show;   // Affichage de la fiche
	F_SplashForm.Update; // Force la fiche à se dessiner complètement

	try
		gb_DicoKeyFormPresent  := True ;
		gb_DicoUseFormField    := True ;
		gb_DicoGroupementMontreCaption := False ;
    if not fb_ReadIni ( gmif_MainFormIniInit ) Then
      ShowMessage ( 'XML file not initalized.' );

		Application.CreateForm(TM_Donnees, M_Donnees);
  Application.CreateForm(TF_FenetrePrincipale, F_FenetrePrincipale);
       // Il n'y a pas de menu donc rootentities est une fenêtre
  if not assigned ( gNod_DashBoard ) then
     p_CreateRootEntititiesForm;

  finally
	end;

	p_RegisterClasses ([]);

	Application.Run;
end.
