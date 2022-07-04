{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukfdConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

const
	TB = #9;
	NL = #13#10;
	DNL = NL + NL;
	NLT = NL + TB;
	DNLT = DNL + TB;
	NLT2 = NLT + TB;
	NLT3 = NLT2 + TB;
	NLT4 = NLT3 + TB;
	NLT5 = NLT4 + TB;
	STARS = '{**************************************************' +
					'***********************}' + NL;

	DFM_KForm =
	'object %s: T%s' + NLT +
	'Left = 200'  + NLT +
	'Top = 120' + NLT +
	'Width = 440' + NLT +
	'Height = 300' + NLT +
	'Caption = ''%s''' + NLT +
//	'Font.Name = ''Arial''' + NLT +
//	'Font.Style = [fsBold]' + NLT +
	'Scaled = False' + NL +
	'end';

	DFM_KSimpleMDI =
	'object %s: T%s' + NLT +
	'Left = 200'  + NLT +
	'Top = 120' + NLT +
	'Width = 440' + NLT +
	'Height = 300' + NLT +
	'Caption = ''%s''' + NLT +
//	'Font.Name = ''Arial''' + NLT +
//	'Font.Style = [fsBold]' + NLT +
	'FormStyle = fsMDIForm' + NLT +
	'Scaled = False' + NLT +
	'WindowState = wsMaximized' + NL +
	'end';

	DFM_KMDIForm =
	'object %s: T%s' + NLT +
	'Left = 200'  + NLT +
	'Top = 120' + NLT +
	'Width = 440' + NLT +
	'Height = 300' + NLT +
	'Caption = ''%s''' + NLT +
//	'Font.Name = ''Arial''' + NLT +
//	'Font.Style = [fsBold]' + NLT +
	'FormStyle = fsMDIForm' + NLT +
	'Menu = mnuMain' + NLT +
	'Scaled = False' + NLT +
	'WindowMenu = mniJanela' + NLT +
	'WindowState = wsMaximized' + NLT +
	'ConfirmClose = True' + NLT +
	'object mnuMain: TKMainMenu' + NLT2 +
	'object mniSistema: TMenuItem' + NLT3 +
	'Caption = ''&Sistema''' + NLT3 +
	'object mniLogon: TMenuItem' + NLT4 +
	'Caption = ''&Logon''' + NLT3 +
	'end' + NLT3 +
	'object mniAlterarSenha: TMenuItem' + NLT4 +
	'Caption = ''Alterar Senha''' + NLT3 +
	'end' + NLT3 +
	'object mniSeparador00: TMenuItem' + NLT4 +
	'Caption = ''-''' + NLT3 +
	'end' + NLT3 +
	'object mniBackup: TMenuItem' + NLT4 +
	'Caption = ''&Backup...''' + NLT3 +
	'end' + NLT3 +
	'object mniConfiguracoes: TMenuItem' + NLT4 +
	'Caption = ''Configurações...''' + NLT3 +
	'end' + NLT3 +
	'object mniConfigurarImpressora: TMenuItem' + NLT4 +
	'Caption = ''Configurar Impressora...''' + NLT3 +
	'end' + NLT3 +
	'object mniSeparador01: TMenuItem' + NLT4 +
	'Caption = ''-''' + NLT3 +
	'end' + NLT3 +
	'object mniSair: TMenuItem' + NLT4 +
	'Caption = ''Sai&r''' + NLT3 +
	'OnClick = mniSairClick' + NLT3 +
	'end' + NLT2 +
	'end' + NLT2 +
	'object mniJanela: TMenuItem' + NLT3 +
	'Caption = ''&Janela''' + NLT3 +
	'object mniCascata: TMenuItem' + NLT4 +
	'Caption = ''&Cascata''' + NLT3 +
	'OnClick = mniCascataClick' + NLT3 +
	'end' + NLT3 +
	'object mniOrganizarIcones: TMenuItem' + NLT4 +
	'Caption = ''&Organizar Ícones''' + NLT3 +
	'OnClick = mniOrganizarIconesClick' + NLT3 +
	'end' + NLT3 +
	'object mniLadoLadoVertical: TMenuItem' + NLT4 +
	'Caption = ''Lado a Lado &Vertical''' + NLT3 +
	'OnClick = mniLadoLadoVerticalClick' + NLT3 +
	'end' + NLT3 +
	'object mniLadoLadoHorizontal: TMenuItem' + NLT4 +
	'Caption = ''Lado a Lado &Horizontal''' + NLT3 +
	'OnClick = mniLadoLadoHorizontalClick' + NLT3 +
	'end' + NLT3 +
	'object mniSeparador02: TMenuItem' + NLT4 +
	'Caption = ''-''' + NLT3 +
	'end' + NLT3 +
	'object mniFecharTodas: TMenuItem' + NLT4 +
	'Caption = ''&Fechar Todas''' + NLT3 +
	'OnClick = mniFecharTodasClick' + NLT3 +
	'end' + NLT3 +
	'object mniMinimizarTodas: TMenuItem' + NLT4 +
	'Caption = ''&Minimizar Todas''' + NLT3 +
	'OnClick = mniMinimizarTodasClick' + NLT3 +
	'end'+ NLT3 +
	'end' + NLT2 +
	'object mniAjuda: TMenuItem' + NLT3 +
	'Caption = ''&?''' + NLT3 +
	'object mniConteudo: TMenuItem' + NLT4 +
	'Caption = ''&Conteúdo...''' + NLT3 +
	'end' + NLT3 +
	'object mniSeparador03: TMenuItem' + NLT4 +
	'Caption = ''-''' + NLT3 +
	'end' + NLT3 +
	'object mniSobre: TMenuItem' + NLT4 +
	'Caption = ''&Sobre...''' + NLT3 +
	'end' + NLT2 +
	'end' + NLT +
	'end' + NL +
	'end';

	DFM_KChild =
	'object %s: T%s' + NLT +
	'Left = 200'  + NLT +
	'Top = 120' + NLT +
	'Width = 440' + NLT +
	'Height = 300' + NLT +
	'Caption = ''%s''' + NLT +
//	'Font.Name = ''Arial''' + NLT +
//	'Font.Style = [fsBold]' + NLT +
	'FormStyle = fsMDIChild' + NLT +
  'Position = poDefaultPosOnly' + NLT +
	'Scaled = False' + NL +
	'end';

	UNIT_KForm =
	'unit %s;' + DNL +
	'interface' + DNL +
	'uses' + NLT +
	'Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,' + NLT +
	'Dialogs, ukfCtrls, ukfClasses;' + DNL +
	'type' + NLT +
	'T%s = class( T%s )' + NLT +
	'private' + NLT2 +
	'{ Private declarations }' + NLT +
	'public' + NLT2 +
	'{ Public declarations }' + NLT +
	'end;' + DNL +
	'var' + NLT +
	'%s: T%s;' + DNL +
	'implementation' + DNL +
	'{$R *.DFM}' + DNL +
	'end.';

	UNIT_KMDIForm =
	'unit %s;' + DNL +
	'interface' + DNL +
	'uses' + NLT +
	'Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,' + NLT +
	'Dialogs, Menus, ukfCtrls, ukfClasses;' + DNL +
	'type' + NLT +
	'T%s = class( T%s )' + NLT2 +
	'mnuMain: TKMainMenu;' + NLT2 +
	'mniSistema: TMenuItem;' + NLT2 +
	'mniLogon: TMenuItem;' + NLT2 +
	'mniAlterarSenha: TMenuItem;' + NLT2 +
	'mniSeparador00: TMenuItem;' + NLT2 +
	'mniBackup: TMenuItem;' + NLT2 +
	'mniConfiguracoes: TMenuItem;' + NLT2 +
	'mniConfigurarImpressora: TMenuItem;' + NLT2 +
	'mniSeparador01: TMenuItem;' + NLT2 +
	'mniSair: TMenuItem;' + NLT2 +
	'mniJanela: TMenuItem;' + NLT2 +
	'mniCascata: TMenuItem;' + NLT2 +
	'mniOrganizarIcones: TMenuItem;' + NLT2 +
	'mniLadoLadoVertical: TMenuItem;' + NLT2 +
	'mniLadoLadoHorizontal: TMenuItem;' + NLT2 +
	'mniSeparador02: TMenuItem;' + NLT2 +
	'mniFecharTodas: TMenuItem;' + NLT2 +
	'mniMinimizarTodas: TMenuItem;' + NLT2 +
	'mniAjuda: TMenuItem;' + NLT2 +
	'mniConteudo: TMenuItem;' + NLT2 +
	'mniSeparador03: TMenuItem;' + NLT2 +
	'mniSobre: TMenuItem;' + DNLT + TB +
	'procedure mniSairClick( Sender: TObject );' + NLT2 +
	'procedure mniCascataClick( Sender: TObject );' + NLT2 +
	'procedure mniOrganizarIconesClick( Sender: TObject );' + NLT2 +
	'procedure mniLadoLadoVerticalClick( Sender: TObject );' + NLT2 +
	'procedure mniLadoLadoHorizontalClick( Sender: TObject );' + NLT2 +
	'procedure mniFecharTodasClick( Sender: TObject );' + NLT2 +
	'procedure mniMinimizarTodasClick( Sender: TObject );' + DNLT +
	'private' + NLT2 +
	'{ Private declarations }' + NLT +
	'public' + NLT2 +
	'{ Public declarations }' + NLT +
	'end;' + DNL +
	'var' + NLT +
	'%s: T%s;' + DNL +
	'implementation' + DNL +
	'{$R *.DFM}' + DNL +
	STARS +
	'{ Code generated by KMDIFormExpert - %2.2d/%2.2d/%4.4d }' + NL +
	STARS + NL +
	'procedure T%s.mniSairClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'Close;' + NL +
	'end;' + DNL +
	'procedure T%s.mniCascataClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoCascade;' + NL +
	'end;' + DNL +
	'procedure T%s.mniOrganizarIconesClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'ArrangeIcons;' + NL +
	'end;' + DNL +
	'procedure T%s.mniLadoLadoVerticalClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoTileVertical;' + NL +
	'end;' + DNL +
	'procedure T%s.mniLadoLadoHorizontalClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoTileHorizontal;' + NL +
	'end;' + DNL +
	'procedure T%s.mniFecharTodasClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoCloseAll;' + NL +
	'end;' + DNL +
	'procedure T%s.mniMinimizarTodasClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoMinimizeAll;' + NL +
	'end;' + DNL +
	STARS +
	'{ End of code generation }' + NL +
	STARS + NL +
	'end.';

	UNIT_KNavMDIForm =
	'unit %s;' + DNL +
	'interface' + DNL +
	'uses' + NLT +
	'Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,' + NLT +
	'Dialogs, Menus, StdCtrls, ExtCtrls, ukfCtrls, ukfClasses, ukfexCtrls, ukrDBCtrls;' + DNL +
	'type' + NLT +
	'T%s = class( T%s )' + NLT2 +
	'pnToolbar: TPanel;' + NLT2 +
	'kdnNavigator: TKFormDBNavigator;' + NLT2 +
	'mnuMain: TKMainMenu;' + NLT2 +
	'mniSistema: TMenuItem;' + NLT2 +
	'mniLogon: TMenuItem;' + NLT2 +
	'mniAlterarSenha: TMenuItem;' + NLT2 +
	'mniSeparador00: TMenuItem;' + NLT2 +
	'mniBackup: TMenuItem;' + NLT2 +
	'mniConfiguracoes: TMenuItem;' + NLT2 +
	'mniConfigurarImpressora: TMenuItem;' + NLT2 +
	'mniSeparador01: TMenuItem;' + NLT2 +
	'mniSair: TMenuItem;' + NLT2 +
	'mniJanela: TMenuItem;' + NLT2 +
	'mniCascata: TMenuItem;' + NLT2 +
	'mniOrganizarIcones: TMenuItem;' + NLT2 +
	'mniLadoLadoVertical: TMenuItem;' + NLT2 +
	'mniLadoLadoHorizontal: TMenuItem;' + NLT2 +
	'mniSeparador02: TMenuItem;' + NLT2 +
	'mniFecharTodas: TMenuItem;' + NLT2 +
	'mniMinimizarTodas: TMenuItem;' + NLT2 +
	'mniAjuda: TMenuItem;' + NLT2 +
	'mniConteudo: TMenuItem;' + NLT2 +
	'mniSeparador03: TMenuItem;' + NLT2 +
	'mniSobre: TMenuItem;' + DNLT + TB +
	'procedure mniSairClick( Sender: TObject );' + NLT2 +
	'procedure mniCascataClick( Sender: TObject );' + NLT2 +
	'procedure mniOrganizarIconesClick( Sender: TObject );' + NLT2 +
	'procedure mniLadoLadoVerticalClick( Sender: TObject );' + NLT2 +
	'procedure mniLadoLadoHorizontalClick( Sender: TObject );' + NLT2 +
	'procedure mniFecharTodasClick( Sender: TObject );' + NLT2 +
	'procedure mniMinimizarTodasClick( Sender: TObject );' + DNLT +
	'private' + NLT2 +
	'{ Private declarations }' + NLT +
	'public' + NLT2 +
	'{ Public declarations }' + NLT +
	'end;' + DNL +
	'var' + NLT +
	'%s: T%s;' + DNL +
	'implementation' + DNL +
	'{$R *.DFM}' + DNL +
	STARS +
	'{ Code generated by KNavMDIFormExpert - %2.2d/%2.2d/%4.4d }' + NL +
	STARS + NL +
	'procedure T%s.mniSairClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'Close;' + NL +
	'end;' + DNL +
	'procedure T%s.mniCascataClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoCascade;' + NL +
	'end;' + DNL +
	'procedure T%s.mniOrganizarIconesClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'ArrangeIcons;' + NL +
	'end;' + DNL +
	'procedure T%s.mniLadoLadoVerticalClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoTileVertical;' + NL +
	'end;' + DNL +
	'procedure T%s.mniLadoLadoHorizontalClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoTileHorizontal;' + NL +
	'end;' + DNL +
	'procedure T%s.mniFecharTodasClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoCloseAll;' + NL +
	'end;' + DNL +
	'procedure T%s.mniMinimizarTodasClick( Sender: TObject );' + NL +
	'begin' + NLT +
	'DoMinimizeAll;' + NL +
	'end;' + DNL +
	STARS +
	'{ End of code generation }' + NL +
	STARS + NL +
	'end.';

	DFM_KNavMDIForm =
	'object %s: T%s' + NLT +
	'Left = 200'  + NLT +
	'Top = 120' + NLT +
	'Width = 440' + NLT +
	'Height = 300' + NLT +
	'Caption = ''%s''' + NLT +
//	'Font.Name = ''Arial''' + NLT +
//	'Font.Style = [fsBold]' + NLT +
	'FormStyle = fsMDIForm' + NLT +
	'Menu = mnuMain' + NLT +
	'Scaled = False' + NLT +
	'WindowMenu = mniJanela' + NLT +
	'WindowState = wsMaximized' + NLT +
	'ConfirmClose = True' + NLT +
	'object pnToolbar: TPanel' + NLT2 +
	'Left = 0' + NLT2 +
	'Top = 0' + NLT2 +
	'Height = 52' + NLT2 +
	'Align = alTop' + NLT2 +
	'BevelInner = bvRaised' + NLT2 +
	'BevelOuter = bvLowered' + NLT2 +
	'Caption = ''''' + NLT2 +
	'object kdnNavigator: TKFormDBNavigator' + NLT3 +
	'Left = 15' + NLT3 +
	'Top = 10' + NLT2 +
	'end' + NLT +
	'end' + NLT +
	'object mnuMain: TKMainMenu' + NLT2 +
	'object mniSistema: TMenuItem' + NLT3 +
	'Caption = ''&Sistema''' + NLT3 +
	'object mniLogon: TMenuItem' + NLT4 +
	'Caption = ''&Logon''' + NLT3 +
	'end' + NLT3 +
	'object mniAlterarSenha: TMenuItem' + NLT4 +
	'Caption = ''Alterar Senha''' + NLT3 +
	'end' + NLT3 +
	'object mniSeparador00: TMenuItem' + NLT4 +
	'Caption = ''-''' + NLT3 +
	'end' + NLT3 +
	'object mniBackup: TMenuItem' + NLT4 +
	'Caption = ''&Backup...''' + NLT3 +
	'end' + NLT3 +
	'object mniConfiguracoes: TMenuItem' + NLT4 +
	'Caption = ''Configurações...''' + NLT3 +
	'end' + NLT3 +
	'object mniConfigurarImpressora: TMenuItem' + NLT4 +
	'Caption = ''Configurar Impressora...''' + NLT3 +
	'end' + NLT3 +
	'object mniSeparador01: TMenuItem' + NLT4 +
	'Caption = ''-''' + NLT3 +
	'end' + NLT3 +
	'object mniSair: TMenuItem' + NLT4 +
	'Caption = ''Sai&r''' + NLT3 +
	'OnClick = mniSairClick' + NLT3 +
	'end' + NLT2 +
	'end' + NLT2 +
	'object mniJanela: TMenuItem' + NLT3 +
	'Caption = ''&Janela''' + NLT3 +
	'object mniCascata: TMenuItem' + NLT4 +
	'Caption = ''&Cascata''' + NLT3 +
	'OnClick = mniCascataClick' + NLT3 +
	'end' + NLT3 +
	'object mniOrganizarIcones: TMenuItem' + NLT4 +
	'Caption = ''&Organizar Ícones''' + NLT3 +
	'OnClick = mniOrganizarIconesClick' + NLT3 +
	'end' + NLT3 +
	'object mniLadoLadoVertical: TMenuItem' + NLT4 +
	'Caption = ''Lado a Lado &Vertical''' + NLT3 +
	'OnClick = mniLadoLadoVerticalClick' + NLT3 +
	'end' + NLT3 +
	'object mniLadoLadoHorizontal: TMenuItem' + NLT4 +
	'Caption = ''Lado a Lado &Horizontal''' + NLT3 +
	'OnClick = mniLadoLadoHorizontalClick' + NLT3 +
	'end' + NLT3 +
	'object mniSeparador02: TMenuItem' + NLT4 +
	'Caption = ''-''' + NLT3 +
	'end' + NLT3 +
	'object mniFecharTodas: TMenuItem' + NLT4 +
	'Caption = ''&Fechar Todas''' + NLT3 +
	'OnClick = mniFecharTodasClick' + NLT3 +
	'end' + NLT3 +
	'object mniMinimizarTodas: TMenuItem' + NLT4 +
	'Caption = ''&Minimizar Todas''' + NLT3 +
	'OnClick = mniMinimizarTodasClick' + NLT3 +
	'end'+ NLT3 +
	'end' + NLT2 +
	'object mniAjuda: TMenuItem' + NLT3 +
	'Caption = ''&?''' + NLT3 +
	'object mniConteudo: TMenuItem' + NLT4 +
	'Caption = ''&Conteúdo...''' + NLT3 +
	'end' + NLT3 +
	'object mniSeparador03: TMenuItem' + NLT4 +
	'Caption = ''-''' + NLT3 +
	'end' + NLT3 +
	'object mniSobre: TMenuItem' + NLT4 +
	'Caption = ''&Sobre...''' + NLT3 +
	'end' + NLT2 +
	'end' + NLT +
	'end' + NL +
	'end';

resourcestring
	sFormsRegKey = 'Software\KnowHow\DelphiEx';

{	sKnowHowHackRegKey = 'Software\KnowHow\DelphiEx'; adjust after all with ukrdConts.pas}

implementation

end.
