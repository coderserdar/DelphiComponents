Precision Language Suite 2.5
============================

Precision Language Suite je sada lokalizaèních nástrojù pro vıvojová prostøedí
Delphi a FreePascal/Lazarus. Umoòuje snadno vytváøet více-jazyèné aplikace ve
všech dostupnıch rámcích (VCL, FireMonkey, LCL, ...) a pro všechny podporované
platformy (Win32, Win64, OSX, ...).

Produkt obsahuje:
* PLS Engine - lokalizaèní funkce a komponenty, urèené pro zaøazení do zdrojovıch kódù Vaší aplikace
* PLS Editor - vıkonnı nástroj pro pøípravu jazykovıch souborù
* PLS Translator - bezplatná verze editoru, urèená pro pøekladatele Vašich jazykovıch souborù

Precision Language Suite vyuívá textovı formát jazykovıch souborù, co umoòuje
snadné pøizpùsobení pøekladù Vaší aplikace potøebám konkrétního uivatele i bez
instalace speciálního software. Umoòuje také pøepnutí jazykového øezu pøímo za
bìhu aplikace.


PLS Engine:
-----------
Lokalizaèní zdrojové kódy obsahují komponentu TplsController, která je urèena
pro snadnou implementaci PLS Enginu do Vašich aplikací a tøídu TLanguageManager,
která (spolu s pomocnımí metodami, funkcemi a promìnnımi) øeší všechny potøebné
lokalizaèní operace. Jejich pouití je velmi snadné, a kromì toho, e je popsáno
v dokumentaci, tak jeho konkrétní implementace je nabízena také PLS Editorem,
coby "návrhy zdrojovıch kódù" pro danı projekt.

Funkce "vestavìné lokalizace" umoòuje lokalizaci Vašich aplikací pøímo za bìhu.
Pouhım kliknutím myší lze vybrat prvek ve Vaší aplikaci a zadat jeho popisek
v aktuálnì zvoleném jazyce. Je vhodná jak pro drobné korektury, tak pro komplexní
lokalizaci externím pøekladatelem, anebo (v tzv. uivatelském módu) také pro
koncové zákazníky Vašich aplikací.


PLS Editor:
-----------
PLS Editor je urèen pro pohodlnou pøípravu pøekladù Vašich aplikací. Jeho souèástí
je editace textù všech jazykovıch øezù v pøehledné tabulce, navigátor poloek,
rychlá filtrace a vyhledávání, monost vyuití pøeddefinovanıch identifikátorù,
filtrù a vlastností pro lokalizaci, hromadné nahrazení textù, evidence Vašich
projektù, konrola pravopisu, a další monosti.

Pøestoe mùe bıt pouit zcela nezávisle na vıvojovém prostøedí (lze zadávat
libovolné, i neexistující identifikátory), nabízí øadu nástrojù pro zvıšení
efektivity práce, jako napø. synchronizace jazykovıch souborù se zdrojovımi
soubory vıvojového prostøedí, pøenos komponent z formuláøù pomocí schránky do
lokalizaèního projektu, návrhy zdrojovıch kódù pro implementaci lokalizace, apod.


PLS Translator:
---------------
Tento nástroj je urèen pro pøekladatele Vašich existujících projektù.
Oproti PLS Editoru má urèitá omezení, ale je dodáván ZCELA ZDARMA.


Licence:
--------
PLS Engine je dodáván jako FREEWARE, podléhající právùm a omezením uvedenım
v licenènímu ujednání. PLS Editor a PLS Translator jsou dodávány jako FREEWARE.
Více informací naleznete v souboru License.txt.


Podporovaná vıvojová prostøedí:
-------------------------------
  Embarcadero Delphi/RAD Studio
  FreePascal/Lazarus IDE


Systémové poadavky pro PLS Editor a PLS Translator:
----------------------------------------------------
Hardware:
  IBM-kompatibilní PC
    1 GB RAM
  240 MB volného místa na disku (20 MB aplikace / 220 MB slovníky)

Operaèní systém:
  Microsoft Windows 2000/XP/Vista/7/8


Instalace:
----------
Spuste instalátor a postupujte dle pokynù prùvodce. 
Pøenosná (portable) verze mùe bıt jednoduše rozbalena do poadované sloky.

Jeliko jsou všechny soubory z balíèku Precision Language Suite instalovány
pod zvolenou sloku, vyberte pro instalaci takovı adresáø, do kterého máte
právo zápisu (tzn. nikoliv "C:\Program Files\", apod.). V opaèném pøípadì
nebudete moci zkompilovat pøíklady, uloit zmìny v pøíkladovıch projektech, apod.

Veškeré zdrojové kódy, dokumentace a pøíklady najdete pøímo ve zvoleném
instalaèním umístìní. Odkaz na sloku "Source" doplòte do nastavení
Delphi (Library Path) a pøípadnì také do souboru Dcc32.cfg (pokud pouíváte
kompilátor pøíkazové øádky).

Komponentu TplsController nainstalujete do Vašeho prostøedí Delphi (nebo Lazarus)
pomocí pøíslušného balíèku, kterı naleznete v podsloce "Packages".

Nástroje PLS Editor (PrecisionLangSuite.exe) a PLS Translator (PLSTranslator.exe)
jsou umístìny v podsloce "Editor".

Na Windows Vista a vyšších musíte bìheme registrace produktu povolit zvıšení
pøístupovıch práv na administrátorská.


Historie zmìn:
--------------
- Version 2.5.2 (30.9.2014)

  PLS Editor:
  * aktualizováno: Definice knihovny tøíd pro komponenty DevExpress
  
  PLS Engine:  
  * pøidáno: Podpora pro Delphi XE6 a XE7
  * opraveno: Naèítání vlastností poloek kolekcí (TCollectionItem), je jsou souèástí objektù (TPersistent) podøízenıch cílové komponentì

- Verze 2.5 (20.10.2013)

  PLS Editor:
  * aktualizováno: "Microsoft Terminology Collection" (nyní je souèástí instalace plnı rozsah tohoto slovníku)
  * pøidáno: Slovník "Precision Apps" (kolekce frází a textù pouívanıch v produktech Precision software & consulting)
  * vylepšeno: Funkce automatickıch pøekladù doznala velmi vıraznıch vylepšení:
       * Podpora pouití libovolného poètu slovníkù najednou, vèetnì monosti stanovení priorit
  	   * Podpora pouití tzv. "API Keys" (autorizaèních klíèù) pro placené pøekladové sluby (klíèe jsou samozøejmì uloeny s pomocí šifrování)
       * Podpora SSL komunikace pro webové pøekladové sluby
       * Lze definovat a pouívat vlastní pøedkladové sluby 
       * Jako slovníky lze pouít soubory formátu TMX (Translation Memory eXchange) 
       * Jako slovníky lze pouít soubory formátu TBX (TermBase eXchange) 
       * Po dokonèení automatického pøekladu je k dispozici protokol s hlášeními a chybami
  	   * Pøeklad jedné lokalizaèní poloky mùe bıt proveden pomocí jednoho zvoleného slovníku (èi sluby) a potvrzovacího dialogu
  	   * Automatickı pøeklad je nyní k dispozici pøímo z editaèního dialogu lokalizaèní poloky
       * Pro všechny pouité slovníky je nyní pouita vyrovnávací pamì (pro rychlejší zpracování pøi dalším pøekladu)
  	   * Pøibyly nové volby pro "Uloení projektu jako slovníku" a "Otevøení slovníku pro editaci"
  	   * Lepší podpora automatického pøekladu textù obsahujících formátovací makra (%d, %s, apod.)
  * pøidáno: Kontrola pravopisu (s vyuitím knihovny Hunspell)
  * pøidáno: Podpora pro skriptovatelné doplòky, vèetnì plného rozhraní k funkcím a datùm PLS Editoru
  * pøidáno: Podpora uivatelsky definovanıch doplòkù (které mohou bıt psány v JavaScriptu, VBScriptu, Pascal Scriptu, a dalších skriptovacích jazycích)
             vèetnì plného rozhraní k funkcím a datùm PLS Editoru, je je dostupné pomocí OLE automatizace.
  * pøidáno: Správce doplòkù (pro uivatelské nastavení pouívanıch doplòkù, klávesovıch zkratek k nim, apod.)
  * pøidáno: Nástroj "Extension Editor" (pro tvorbu novıch doplòkù)
  * pøidáno: Záloky na lokalizaèní poloky
  * pøidáno: Turecká lokalizace PLS Editoru
  * vylepšeno: Dialog pro vyhledání nyní pøebírá vıchozí text z políèka pro filtrování poloek
  * vylepšeno: Dialog pro vyhledání a nahrazení textu nyní obsahuje monost brát v úvahu (anebo ignorovat) identifikátory poloek
  * vylepšeno: V poli pro filtrování poloek nyní mùete pouít klávesu Enter pro postupné nastavování se na jednotlivé poloky vyhovující filtru
  * opraveno: Skryté sloupce (jazyky) ji nejsou brány v úvahu pøi hledání, ani nahrazování textu
  * vylepšeno: Byl implementován novı analyzátor (parser) zdrojovıch souborù formuláøù (DFM, LFM, FMX), co vyøešilo nìkteré drobné chyby nalezené v pøedchozích verzích editoru
  * zmìnìno: Syntaxe sekce pro deklaraci známıch vlastností VCL komponent v souborech knihoven (.PLP). Nyní je tato sekce ji ignorována,
             take pokud jste mìli definovány vlastní tøídy a vlastnosti, definujte je prosím znovu - v novém "Správci knihovny tøíd".
  * pøidáno: Správce knihovny tøíd - umoòuje pohodlnou editaci seznamu známıch a ignorovanıch tøíd komponent a jejich vlastností, je jsou pouívány pøi analıze zdrojovıch souborù formuláøù (DFM, FMX, LFM)
  * pøidáno: Utilita "Class Library Generator" pro vytvoøení úvodního konceptu uivatelské knihovny tøíd (pro analızu souborù DFM/FMX/LFM) na základì zadanıch zdrojovıch souborù 
  * pøidáno: Podpora novıch verzí vıvojového prostøedí Delphi XE3, XE4 a XE5 (správné zpracování jmennıch prostorù pro importy øetìzcovıch konstant ze sekcí "resourcestring")
  * pøidáno: Vıchozí jazyk projektu mùe bıt nyní zmìnìn také pomocí kliknutí pravého tlaèítka myši spolu s klávesou Shift na zvolenı sloupec jazyka
  * pøidáno: Záhlaví sloupce vıchozího jazyka je nyní zobrazeno tuènì 
  * vylepšeno: Logické øazení indexovanıch poloek (napø. collections)
  * vylepšeno: Volba stylu zobrazení "tagù"
  * pøidáno: "Tagy" mohou bıt nyní pøiøazeny pøímo v dialogu pro editaci poloky
  * pøidáno: Popisky "tagù" (pro lepší pochopení jejich vıznamu)
  * pøidáno: Monost pøiøadit do projektu všechny zdrojové soubory z vybrané sloky (vèetnì podsloek)
  * pøidáno: Indikace zmìn ve zdrojovıch souborech od posledního otevøení projektu (nové poloky, smazané poloky, atd.)
  * pøidáno: Upozornìní na zmìnu obsahu zdrojovıch souborù jinou aplikací v prùbìhu práce s projektem 
  * pøidáno: Podpora vıstupních sestav (reportù)
  * vylepšeno: Podpora práce se schránkou (nyní lze kopírovat a vkládat poloky také jako prostı text
  * vylepšeno: Export poloek nyní obsahuje jen opravdu viditelné poloky a jazyky (sloupce) - døíve byly exportovány sice jen viditelné poloky, ale vèetnì skrytıch jazykù (sloupcù)
  * pøidáno: Export do formátu HTML
  * opraveno: Import souborù ve formátu FastReport (oprava zpracování kódování UTF-8)
  * opraveno: Generace unity LangConsts.pas:
       * Jestlie nebyly definovány ádnı konstanty, procedura LanguageChanged byla deklarována v sekci "interface", ale chybìla v sekci "implementation"
       * Indexované konstanty jsou nyní limitovány na max. 1024 poloek v poli, aby se zabránilo deklaracím nadmìrnì rozsáhlıch polí a problémùm s vyuitím pamìti
  * vylepšeno: Návrhy zdrojovıch kódù (zobrazují se kontextovì k platformì projektu, byla pøidána podpora vlastních šablon)
  * opraveno: Ošetøení vlastních názvù jazykù v dialogu "Nastavení projektu"
  * opraveno: Monost definice více ne jedné dodatkové sloky pro ukládání kopií jazykovıch souborù
  * opraveno: Info ve stavovém øádku (celkovı poèet poloek) nebylo aktualizováno po odebrání jazyka nebo zdrojového souboru
  * a další drobná vylepšení a opravy ...

  PLS Translator:
  * Novı nástroj v rámci Precision Language Suite, jen je urèen pro pøekladatele Vašich existujících projektù.
    Je poskytován ZCELA ZDARMA a v porovnání s plnohodnotnım PLS Editorem má následující omezení:
      - Pøidávání, mazání a duplikování poloek není povoleno
      - Funkce Import není k dispozici (oproti tomu Export dostupnı je)
      - Funkce vloení ze schránky nepodporuje objekty zkopírované z prostøedí Delphi/Lazarus
      - Návrhy zdrojovıch kódù nejsou k dispozici
      - Seznam zdrojovıch souborù je skryt, zdrojové soubory nejsou analyzovány, ani synchronizovány (ale v projektovém souboru samozøejmì zùstávájí zachovány)
      - Správce knihovny tøíd není k dispozici
      - Volby pro pøidání pøeddefinovanıch konstant a zpráv nejsou k dispozici
      - Zdrojovı soubor LangConsts.pas není generován a veškeré volby na nìj navazující jsou skryty (ale v projektovém souboru samozøejmì zùstávájí zachovány)
      - Uivatel mùe doèasnì zmìnit vıchozí jazyk projektu, ale tato zmìna se neprojeví v uloeném projektovém souboru
      - Dodateèné sloky pro uloení kopií jazykovıch souborù .LNG jsou ignorovány (ale v projektovém souboru samozøejmì zùstává tento seznam zachován)
      - Volby pro spuštìní vlastních utilit "pøed otevøením" a "po uloení" projektu nejsou k dispozici (ale samozøejmì zùstávají zachovány v projektovém souboru)
      - Monost volby platformy (VCL/FMX/LCL) je v dialogu "Nastavení projektu" dostupná jen pøi vytváøení nového projektu
      - Nové projekty lze vytváøet, ale poloky lokalizace lze do projektu pøidávan jen zaøazením existujících .LNG souborù
      - Všechny vıše zmínìné funkce jsou vypnuty rovnì ve skriptovatelném rozhraní, take doplòky, které jsou urèeny pro PLS Editor lze pouívat, ale jejich funkènost je omezena na monosti PLS Translatoru

  PLS Engine:
  * pøidáno: Podpora pro Delphi XE3, XE4 a XE5
  * pøidáno: Podpora 64bitového kompilátoru pro Windows
  * pøidáno: Podpora FireMonkey 
  * pøidáno: Podpora Metropolis UI 
  * pøidáno: Podpora módu "Object pascal" pro Lazarus/FPC
  * pøidáno: Komponenta TplsController pro ještì snadnìjší implementaci PLS Enginu do Vašich aplikací
  * pøidáno: Metoda TLanguageManager.LangForm - jedná se o alias metody TLanguageManager.LangVCL, kterı byl zaveden proto, aby nedocházelo ke zmatkùm pøi pouití v jinıch vıvojovıch rámcích (FMX, LCL, atd.)
  * pøidáno: Metoda TLanguageManager.LangString - pracuje obdobnì jako GNU gettext, take vrací pøeklad øetìzce pøedaného funkci coby parameter
  * pøidáno: Globální funkce _ (podtrítko) - coby alias pro metodu LangString globálního LanguageManageru (kvùli podpoøe stylu zápisu funkce GNU gettext)
  * vylepšeno: Procedura _EscapeText je nyní v unitì plsLangMan deklarována jako public 
  * vylepšeno: V unitì plsDialogs jsou nyní k dispozici pøetíené funkce MessageDlg a InputQuery (bez postfixu LM)
  * vylepšeno: Všechny zdrojové soubory PLS Enginu byly pøepracovány tak, aby umoòovaly pouití stejného zdrojového kódu pro všechny podporované rámce (VCL, LCL, FMX)
  * zmìnìno: Funkce vestavìné lokalizace je nyní poskytována zdarma, jako souèást PLS Enginu (doposud byla k dispozici jen po zakoupení PLS Editoru)
  * zmìnìno: Podpora pro Delphi 5 a 6 byla zrušena
  * a další drobná vylepšení ...


- Verze 2.2.7 (25.09.2011)

  PLS Editor:
  * pøidáno: Japonská lokalizace
  * pøidáno: tag "naposledy importováno" - pøiøazuje se všem importovanım polokám (a polokám vloenım ze schránky), pøièem je automaticky odstranìn pøi zavøení daného projektu
             (pøiøazování tohoto tagu mùete vypnout volbou "Nastavovat 'naposledy importováno'" v pøedvolbách editoru)
  * pøidáno: tag "neukládat" - je implementován pøedevším pro zamezení ukládání nechtìnıch vlastností z VCL formuláøù do vıslednıch jazykovıch souborù
  * pøidáno: volba pro podmínìnı import, která zamezí pøepisování ji pøeloenıch poloek - pøi tomto zpùsobu importu se budou pøepisovat jen prázdné poloky a poloky s pøiøazenım tagem "pøeloit"
  * pøidáno: skriptovatelné rozhraní pro import/export (jazykové xml soubory komponent FastReport jsou podporovány coby prvotní pøíklad vyuití)
  * vylepšeno: pøi importu DFM souborù (a vkládání VCL komponent ze schránky) jsou nyní automaticky naèítány také kolekce (vlastnosti typu TCollection a jejich pøidruené lokalizovatelné vlastnosti)
  * vylepšeno: vlastnosti typu TShortCut jsou nyní automaticky naèítány do editoru v textové podobì (tedy napø. "Ctrl+C", apod.).
  * vylepšeno: byla pøidána volba "Ukládat poloky vdy v poøadí dle identifikátoru"
  * vylepšeno: tip s celım textem buòky se nyní zobrazuje i v prvním sloupci s identifikátorem
  * vylepšeno: definice vlastnosti komponent pro automatickı import byla rozšíøena o nìkolik komponent spoleènosti Bergsoft (http://www.bergsoft.net/)
  * opraveno: procedura LanguageChanged je nyní v souboru LangConsts.pas rozdìlována na více procedur, pokud je poèet konstant pøíliš velkı na to, aby mohl bıt zpracován v rámci jedné procedury
  * opraveno: podpora naèítání DFM souborù s definicí TFrame a DFM souborù formuláøù obsahujících vloené objekty typu TFrame

  PLS Engine:
  * pøidáno: kompatibilita s Delphi XE2
  * zmìnìno: vıchozí hodnota vlastnosti TLanguageManager.AllowSpecialProps je nyní True, a to z dùvodu zpracování speciálních vlastností (jako napø. ShortCut) v uvatelsky pøívìtivé podobì v rámci editoru.
  * pøidáno: podpora vloenıch rámcù (TFrame) pro "vestavìnou lokalizaci"

- Verze 2.2.6 (21.07.2011)

  PLS Editor:
  * pøidáno: Italská lokalizace
  * pøidáno: Volba pro pøepnutí tagu u vybranıch poloek pøi dvojitém kliknutí na barvu tagu (myšleno v seznamù tagù)
  * vylepšeno: Filtrování poloek nyní bere v úvahu skryté jazyky (poloka nebude zobrazena ve filtrovaném seznamu, i kdy vyhovuje filtru, pokud je její jazyk skrytı)

  ... plus zmìny z prùbìnıch beta verzí a opravnıch vydání (v2.2.4.21 a v2.2.5.2):
  * vylepšeno: Nová volba "Zobrazit poloky bez tagù", která je k dispozici v záloce "Tagy"
  * vylepšeno: Novı filtr v navigátoru pro zobrazení "jen lokalizovanıch poloek"
  * vylepšeno: Vyhledávání v polokách dle masky. Mùete vyuít znaky * a ?
  * vylepšeno: Indikace pro vyèištìní filtru v polích pro vyhledávání (ikona køíku)
  * opraveno: Chyba "Access violation ..." pøi pokusu o pøidání nové poloky v situaci, kdy je seznam poloek tøídìn podle jednoho z jazykù. Tato chyba se neobjevvovala v situaci, kdy byl seznam tøídìn standardnì dle identifikátorù poloek.
  * opraveno: Import z DFM souborù - naèítání vlastností typu TStrings, jejich hodnoty obsahují apostrofy anebo speciální znaky
  * opraveno: Import DFM souborù, kde nìkteré hodnoty vlastností zaèínají kódovanımi znaky (napø. #286,#1086, atd.). Vlastnosti, jejich hodnoty zaèínaly standardním znakem, se pøevádìly správnì.
  * opraveno: Zobrazování stavu zaškrtnutí pro vybranou pøekladovou slubu nebo vybranı uivatelskı slovník na Windows XP/Vista/7.

  PLS Engine:
  * opraveno: Podpora pro lokalizaci strukturovanıch objektù (napø. Collection s polokami, které obsahují další Collection, atd.)

  ... plus zmìny z prùbìnıch beta verzí a opravnıch vydání (v2.2.4.21 a v2.2.5.2):
  * opraveno: Chybná direktiva podmínìného pøekladu pro Delphi 6 v demonstraèních projektech LangSuiteDemo a EmbeddedEditDemo (která zpùsobovala chybu kompilace "an unknown method ValueFromIndex" pro objekt TStringList).


- Verze 2.2.4.19 (02.01.2011)

  PLS Editor:
  * pøidáno - pøekladové slovníky "Microsoft Terminology Collection"
  * pøidáno - pøíklady a postupy: "Lokalizace komponent DevExpress", "Lokalizace konstant BusinessSkinForm" a "Pøechod z komponent DKLang" (viz "Nápovìda - Pøíklady a postupy")
  * pøidáno - podpora tzv. externích jazykovıch souborù (tedy souborù ve standardním formátu LNG, ovšem s odlišnou pøíponou, které lze v editoru normálnì upravovat,
              ale jejich naètení v lokalizované aplikaci je na volbì vıvojáøe, a nikoliv na tøídì TLanguageManager). Takové jazykové soubory lze s úspìchem vyuít
              pro lokalizaci komponent, které mají implementovány vlastní mechanismy lokalizace (napø. BusinessSkinForm, DynamicSkinForm, a další).
              Spoluautorem tohoto øešení je pan Calin Paiusan ze spoleènosti Soft Consulting West Team, www.softwestteam.ro.
  * vylepšeno - monosti importu byly rozšíøeny o naètení projektù a lokalizaèních souborù v následujících formátech:
                DKLang, Ini, Seznamy konstant (ve formì identifikátor=text), a interní formáty LNG a LNGU
  * pøidáno - funkce pro hromadné "potvrzení" hodnot ze zdrojovıch souborù (formuláøù, zdrojovıch øetìzcù) - tzn. jejich pøevzetí a uloení do jazykovıch souborù
  * pøidáno - monost oznaèení jazyka pøíznakem "bi-directional"
  * vylepšeno - pøeklad z uivatelskıch slovníkù nyní zachovává velká/malá písmena na zaèátku vìty, co platí také pro znaky akcelerátorù (&)
  * vylepšeno - pøejmenování formuláøù a komponent pøímo v navigátoru poloek
  * vylepšeno - øazení uivatelskıch slovníkù v nabídce dle abecedy
  * vylepšeno - seznam vıchozích dostupnıch jazykù pro editor byl doplnìn o jazyky obsaené v "Microsoft Terminology Collection"
  * vylepšeno - rychlost naèítání a ukládání projektù, rychlost synchronizace se zdrojovımi kódy (podpora práce s velmi rozsáhlımi projekty)
  * opraveno - import souborù ve formátech .RC a .DRC, které byly upraveny v novìjších verzích Delphi nástrojem Translation Editor.
               Translation Editor ukládá upravené soubory s pùvodní pøíponou .RC, ale jejich formát zmìní na .DRC a vytvoøí doplòkovı soubor .RCN obsahující informace,
               které byly pùvodnì uloeny pøímo v souboru .RC (v jeho standardním formátu). Takto zmìnìné soubory zdrojù je ji tedy moné také importovat do Precision Language Suite.

  PLS Engine:
  * pøidáno - podpora vıvojového prostøedí FreePascal/Lazarus
  * pøidáno - vlastnost TLanguageManager.BiDiMode
  * pøidáno - vlastnost TLanguageManager.ActiveFiles, která obsahuje seznam aktuálnì naètenıch jazykovıch souborù
  * vylepšeno - parametr "ButtonElevation" pro funkci MessageDlgLM (autorem doplnìní je ouiouioui, www.toutenvrac.org)
  * pøidáno - unita "xplsDKLang.pas", která obsahuje funkce pro jednodušší pøechod z lokalizaèních komponent DKLang (viz také "Nápovìda - Pøíklady a postupy - Pøechod z komponent DKLang")
  * pøidáno - podpora pro vıvojové prostøedí FreePascal/Lazarus v rámci "vestavìné lokalizace"
  * pøidáno - podpora ukládání zmìn do souborù v komprimovaném formátu (lngz) v rámci "vestavìné lokalizace"


- Verze 2.1.3.17 (17.12.2010)

  PLS Editor:
  * pøidáno - Maïarská lokalizace
  * pøidáno - Znaèky (tagy) pro lokalizované poloky, vèetnì podpory automatického oznaèování. Mùete pouívat vıchozí tagy ("pøeloit", "pøeloeno automaticky",
              "nelokalizovat", "neukládat do langconsts"), stejnì tak, jako mùete definovat tagy vlastní, pomocí vytvoøení uivatelské knihovny tagù (znaèek).
  * pøidáno - Uivatelské slovníky pro automatické pøeklady. Pøizpùsobitelná pravidla pøekladu a monost souèasného pouití s webovou pøekladovou slubou.
              Jakıkoliv projekt Precision Language Suite mùe bıt pouit coby pøekladovı slovník (za slovníky jsou automaticky brány projekty uloené ve sloce "Templates\Dictionaries").
  * pøidáno - Lokalizace resource strings (zdrojovıch øetìzcù). Tyto øetìzce lze importovat ze souborù typu PAS, INC, DPR, RC a DRC.
  * pøidáno - Jednorázovı import Delphi formuláøù (DFM souborù), implementovanı jako doplnìk k automatické synchronizaci VCL souborù a ke vkládání komponent ze schránky.
  * vylepšeno - Import více souborù najednou, vèetnì automatického pøidání pøíslušného jazyka, definovaného v importovanıch souborech zdrojù (.RC soubory)
  * vylepšeno - Automatická synchronizace VCL souborù nyní podporuje i formáty PAS, INC a DPR
  * pøidáno - Podpora pro pøedzpracování a následného zpracování projektu. Pøed otevøením, a po uloení projektu, lze spustit libovolnou definovanou externí aplikaci.
  * pøidáno - Monost ukládání jazykovıch souborù v komprimovaném (pk-zip kompatibilním) formátu (pøípona LNGZ).
              Toto platí pro ukládání souborù do doplòkovıch umístìní (viz "Nastavení projektu"). Vaše zdrojové lokalizaèní soubory zùstávají ve standardním LNG formátu.
  * vylepšeno - Funkce "Duplikovat" nyní vytváøí správné poøadí indexù pro "konstanty typu pole", i pro ostatní poloky s èíselnım oznaèením na konci identifikátoru
  * vylepšeno - Monost uloení vlastních filtrù pro Navigátor poloek (kliknutím na ikonu lupy v poli pro vyhledávání dle textu)
  * vylepšeno - Informace o verzi jazykového souboru (autor, èíslo verze, atd.) jsou nyní synchronizovány s informacemi o verzi projektu
  * vylepšeno - Monost otevøení souborù knihoven (.PLP) pro úpravy pøímo z prostøedí editoru
  * vylepšeno - Byly pøidány horké klávesy pro aktivaci vyhledávacího pole (Ctrl+G) a vypnutí filtru poloek (Ctrl+Q)
  * pøidáno - Volba "zahájit editaci pøi psaní", kterou lze aktivovat namísto "inkrementálního vyhledávání", a která umoní zahájit editaci poloky bez pøedchozího stisku klávesy F2
  * opraveno - Podpora relativních cest k souborùm VCL zaøazenım do projektu
  * opraveno - Do generovaného souboru LangConst.pas je nyní správnì zaøazována unita SysUtils, pokud v projektu vyuíváte "konstanty typu pole"
  * opraveno - Dlouhé øetìzce (delší ne 255 znakù) jsou nyní do souboru LangConsts.pas generovány správnì (vèetnì rozdìlení na více øádkù)
  * opraveno - Øetìzce s apostrofy (') jsou nyní do souboru LangConsts.pas generovány správnì

  PLS Engine:
  * pøidáno - Podpora Delphi XE
  * pøidáno - Lokalizace resource strings (zdrojovıch øetìzcù). Viz funkce LangResourceStr v unitì plsLangMan.pas, projekt LangSuiteDemo, a funkce Import v editoru.
  * pøidáno - Podpora naèítání jazykovıch souborù v komprimovaném (pk-zip kompatibilním) formátu (pøípona LNGZ). Tato funkce se aktivuje definováním direktivy podmínìného pøekladu "PLS_LNGZ".
  * opraveno - doplnìní funkcí GetLangPrimaryCode a GetLangSubCode dle aktuální specifikace jazykovıch identifikátorù (viz http://msdn.microsoft.com/en-us/library/dd318693(VS.85).aspx)
  * pøidáno - funkce GetLangCountry, která vrací celı øetìzcovı identifikátor jazyka a zemì (napø. "cs-CZ")
  * pøidáno - direktiva {$M+}
  * pøidáno - Podpora naèítání jazykovıch souborù uloenıch v kódování UTF-8 i pro Delphi verze 7 a 2007.
              Pro tyto verze Delphi ji tedy není potøeba konvertovat jazykové soubory vytvoøené editorem do pøíslušné kódové stránky daného jazyka.


- Verze 2.0.1.2 (10.06.2010)

  PLS Editor:
  * pøidáno - Ruská lokalizace
  * pøidáno - Francouzská lokalizace
  * pøidáno - Monost pøekladu pomocí sluby Bing (Microsoft Translator)
  * pøidáno - Import a Export poloek z a do formátu CSV (jako další monost
              zajištìní pøekladu externím pøekladatelem)
  * pøidáno - Monost ukládání jazykovıch souborù i do dalších zvolenıch sloek
              (pøi kadém uloení projektu)
  * vylepšeno - Podpora zdìdìnıch ("inherited") formuláøù a komponent pøi naèítání
                VCL souborù, vèetnì pøíkladu pouití takovıchto formuláøù pøi
                lokalizaci samotné
  * vylepšeno - Podpora naèítání VCL souborù v binárním formátu
  * vylepšeno - Kromì zálohování jazykovıch souborù se nyní pøi ukládání projektu
                zálohuje také soubor konstant ("LangConsts.pas")
  * opraveno - Podpora speciálních znakù (napø. èeština) pøi naèítání VCL souborù
  * opraveno - Pøi naèítání VCL souborù jsou nyní podporovány hodnoty vlastností,
               které obsahují více øádkù textu
  * opraveno - Podpora naèítání vlastností typu "Items.Strings", "Items.Text",
               "Lines.Strings" a "Lines.Text" z VCL souborù
  * opraveno - Kritická chyba ("Access violation ...") ve funkci "Nahradit text"
  * vylepšeno - Pomocí funkce "Nahradit" lze nyní hromadnì zmìnit také názvy
                identifikátorù (formuláøù, komponent, vlastností)
  * opraveno - Textové popisky v dialogu "Najít" nyní odpovídají aktuálnì vybranému
               jazyku uivatelského prostøedí i pøi jeho prùbìné zmìnì
  * a další drobná vylepšení a opravy

  PLS Engine:
  * pøidáno - Metoda "plsDialogs.InputQueryMemoLM" pro dotaz na více-øádkovı text
  * pøidáno - Podpùrné metody pro "vestavìnou lokalizaci"
  * pøidáno - Nové pøíklady (vestavìná lokalizace, lokalizace "od nuly",
              rychlost naèítání lokalizovanıch textù)
  * pøidáno - Pøíklad lokalizace zdìdìnıch ("inherited") formuláøù (v nápovìdì
              produktu)
  * pøidáno - Vestavìná lokalizace: tato novì pøidaná funkce umoòuje lokalizaci Vašich aplikací pøímo za bìhu.
              Pouhım kliknutím myší lze vybrat prvek ve Vaší aplikaci a zadat jeho popisek
              v aktuálnì zvoleném jazyce. (více informací naleznete v nápovìdì k produktu)


- Verze 1.0.1.83 (22.09.2009)

  * první veøejnì distribuovaná verze produktu


Pouité komponenty a knihovny tøetích stran:
--------------------------------------------

PLS Editor:
- Embarcadero Delphi Professional, http://www.embarcadero.com/
- TMS Component Pack Professional, http://www.tmssoftware.com
- VirtualTree, http://www.soft-gems.net
- Precision Language Suite, http://www.be-precision.com/products/precision-langs/
- pdScript (embedded), http://www.be-precision.com/products/pdscript/
- PNG Components, http://www.thany.org
- THDDInfo Delphi component, http://artsoft.nm.ru, artsoft@nm.ru
- SynEdit, http://synedit.sourceforge.net/
- TmPasParser, Martin Waldenburg
- SciZipFile, Patrik Spanel - scilib@sendme.cz
- MSXML SDK, http://msdn.microsoft.com/en-us/library/ms760399(VS.85).aspx
- Open SSL, http://www.openssl.org/
- Hunspell library, http://hunspell.sourceforge.net/ (vèetnì rùznıch slovníkù tøetích stran)
- Google Translate API, https://developers.google.com/translate/
- Microsoft Translator HTTP interface (REST), http://www.microsofttranslator.com/dev/
- Microsoft Terminology Collection, http://www.microsoft.com/language/en-us/default.aspx
- Flot, http://www.flotcharts.org/
- InnoSetup, http://www.jrsoftware.org
- ISTool, http://www.istool.org/
- IcoFX, http://icofx.ro/
- Rumshot, http://www.shellscape.org/rumshot/
- Fugue Small Icons, http://www.pinvoke.com/
- Silk Icons, http://www.famfamfam.com/lab/icons/silk/
- Developers Icons, http://sekkyumu.deviantart.com/art/Developpers-Icons-63052312
- Human O2 Icons, http://schollidesign.deviantart.com/art/Human-O2-Iconset-105344123
- Oxygen Icons, http://www.oxygen-icons.org/

PLS Engine:
Lokalizaèní zdrojové kódy nevyuívají ádné souèásti tøetích stran,
ale pouze kódy standardnì obsaené ve vıvojovém prostøedí Delphi (resp. FPC/Lazarus).
Vıjímku tvoøí volitelná podpora souborù v komprimovaném formátu (SciZipFile.pas):
- SciZipFile, Patrik Spanel - scilib@sendme.cz


Podìkování:
-----------
- Komponenta TplsController je vyvíjena ve spolupráci s MiTeC company (Michal Mutl), http://www.mitec.cz/
- Implementace podpory pro zobrazení ikon "štítù" pøi poadavku na zvıšení oprávnìní v unitì plsDialogs - Alexandre Veuillet, http://www.toutenvrac.org/


Kontakt:
--------
Precision software & consulting
e-mail:  info@be-precision.com
www:     http://www.be-precision.com
podpora: www.be-precision.com/support
         support@be-precision.com
fórum:   www.be-precision.com/forum
rss:     www.be-precision.com/rss_cz.xml


========================================================
Copyright (c) 2008-2014  Precision software & consulting
Všechna práva vyhrazena.
