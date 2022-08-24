Galician dictionary for the spell checker MySpell, that is used in
OpenOffice.org.

1. Copyright
2. Content
3. Installation
4. Normas
5. License
6. leme.txt
7. Copying


1. Copyright

This is a MySpell dictionary for galician, using the "minimos" standard. It is
based in the Ispell dictionary created by André Ventas and Ramón Flores
(http://ispell-gl.sourceforge.net/ispell-gl-en.html) that is released under the
terms of the GNU GPL (version 2). The modifications made to allow it works with
MySpell were made by Ramón Flores, being the modified dictionary covered by the
GNU GPL (version 2).


2. Content

This package has the following files:

	gl_ES.dic,  is a galician root list.
	gl_ES.aff,  has the rules to derive words from the root list.
	leme.txt,   a translation of this file to galician.
	readme.txt, this file.
	COPYING	    is the GNU General Public Licence.
	LICENZA.txt is a galician translation of the GNU GPL.
	normas.txt  has some linguistic information (in galician).
	


3. Installation in OpenOffice.org

   a. Close all open OOorg windows and the Quickstarter, before installing and
      editing the dictionary.lst file.
   b. if OOorg1.0.0 then copy gl_ES.dic and gl_ES.aff in the directory,
      OpenOffice.org/user/wordbook/
   c. if OOorg1.0.1 then copy gl_ES.dic and gl_ES.aff in the directory,
      OpenOffice.org/share/dict/ooo/
   d. (See note) Open the dictionary.lst file, that is in that same directory,
      using any text editor and add the following line:
           DICT gl ES gl_ES
      
      This line tells OOorg to register the affix file gl_ES.aff and the
      wordlist gl_ES.dic to the locale, gl ES, which is Galician (Spain).

      The specific fields of this line are:

      Field 1: Entry Type. "DICT"  logicaly means dictionary
      Field 2: Language code from Locale "gl" or "en" or "pt" ... 
      Field 3: Country code from Locale "ES" or "GB" or "PT" ... 
      Field 4: Root name of Dictionary "english" or "en_US" or ... (do not add
               the .aff or .dic extensions to the name)

   e. Now, start up OpenOffice.org, and go to:
           Tools->Options->LanguageSettings->WritingAids 
      Hit "Edit" and use the pull down menu to select galician and then make 
      sure to check the MySpell Spell Checker for that locale.


IMPORTANT NOTE
 Nowadays, 09/30/02, the galician language is not supported in OpenOffice so it
is necessary to asociate the galician files to another supported locale, for
example af_ZA Afrikaans(South Africa), or sq_AL Albanian(Albania). In order to
do it the items d. and e. are a bit different:
 
  d. Open the dictionary.lst file, that is in that same directory, using any
     text editor and add the following line:
         DICT af ZA gl_ES           ( or   DICT sq_AL)
 
  e. Now, start up OpenOffice.org, and go to:
         Tools->Options->LanguageSettings->WritingAids 
     Hit "Edit" and use the pull down menu to select  afrikaans (or albanian) 
     and then make sure to check the MySpell Spell Checker for that locale.

4. Normas

Este dicionário segue a normativa de mínimos ortográficos, baseando-se nas
seguintes duas fontes:

   · NOVA GRAMÁTICA para a aprendizaxe da língua de Xoán Xosé Costa Casas, 
     Mª dos Anxos González Refoxo, César Carlos Morán Fraga e Xoán Carlos
	Rábade Castiñeira, editada por Vía Láctea en 1988.
	
   · DICIONÁRIO DA LÍNGUA GALEGA de Isaac Alonso Estravís, editado por
     Sotelo Blanco en 1995.


Pode-se achar información en liña sobre esta normativa no site de "La 
questione de la lingua":
	http://www.multimania.com/questione/documentos/documentos.html
	
e no site da CIG	
	http://www.galizacig.com/ficheiros/html/normaliza/normaliza.htm
	
A normativa de mínimos ten un espírito conciliador, e admite distintas 
variantes para unha mesma palabra. Con todo a NOVA GRAMÁTICA da como 
preferentes algunhas delas, asi na apresentación di:

	En certas terminacións e ditongos admiten-se como plenamente válidas
	as duas variantes por seren amabas lexítimas, estimadas, como  
	preferentes por parte da APLL (Asociación de Profesores de Língua e
	Literatura) aquelas que indicamos en primeiro lugar:
	
		-eo   / -eio  (freo/freio, creo/creio, etc.)
		-ea   / -eia  (frea/freia, idea/ideia, etc.)
		-an   / -ao   (chan/chao, irmán/irmao, etc.)
		-e    / -en   (marxe/marxen, viaxe/viaxen, etc.)
		-oa   / -o    (avoa/avó, moa/mó, etc.; máis só)
		oi(t) / ui(t) (loita/luita, moito/muito, roibo/ruibo[1], etc)

	O mesmo poderíamos dicer de formas xa gramaticais como ti/tu e
	imos/vamos.
	
	A respeito de outros casos como as terminacións -zón/-ción 
	(nazón/nación), -són/-sión (concisón/concisión), -tón/-tión
	(dixestón/dixestión) e -xón/-xión (reflexón/reflexión), consideran-se
	noutro plano diferente aos anteriores, por se tratar agora de escoller
	entre as formas lexítimas pero social e institucionalmente menos
	implantadas (-zón, -són, -tón, -xón), e formas propriamente
	castellanizadas ainda que social e institucionalmente máis empregadas
	(-ción, -sión, -tión, -xión). Aqui adopta-se como critério o das
	Normas de 80 aludidas[2]: "recomenda-se a potenciación das formas
	primeiras [as lexitimas], coas limitacións ou ritmo que aconselle o
	proceso gradual do ensino e da sensibilidade social".
	
	Seguindo estas pautas, decidimo-nos a utilizar as terminacións xenuínas
	en certas palabras que apresentan unha particular evolución fonética
	que as afasta da estrutura latina e as aproxima da sua forma romance
	histórica: eleizón, entoazón, lizón, perfeizón, -uizón (constituizón,
	contribuizón, distribuizón, etc.) xerazón, etc.; confisón, procisón,
	profisón. Vexa-se sobre todo isto, o Apéndice 2 do libro, no primeiro
	apartado das terminacións.[3]
		
	[1] Nós seguindo o dicionário antes citado damos por válida: roivo.
	[2] Fai alusión as "Normas ortográficas do idioma galego", normas 
	    ortográficas aprobadas pola consellaria de cultura da xunta 
	    preautonómica en 1980. Poden-se consultar en:
	    http://www.multimania.com/questione/documentos/normas_xunta_preautonomica.html
	[3] Este apéndice pode-se achar tamén no site de "La questione de la
	    lingua":
	    http://www.multimania.com/questione/documentos/cuadro_comparativo.html
	
	
A flexibilidade da normativa de mínimos non lle acai ben a un corrector
ortográfico, e asi non parece coerente que nun mesmo texto o corrector dé 
por válidas duas, ou mesmo tres, variantes distintas da mesma palabra,
por ex. idea e ideia. Por iso nós no noso dicionário só damos por válida 
unha das posíbeis variantes, seguindo os seguintes critérios:

	1) Nas terminacións antes citadas seguimos o critério da APLL, i.e.
	   -eo, -ea, -an, -e, -oa, oi(t)
 	2) Tamén para as terminacións -zón/-ción, -són/-sión, -tón/-tión  e
	   -xón/-xión, pretendemos seguir o critério da APLL. Mais dado que
	   resulta un tanto ambigüo, o critério real que seguimos foi dar por
	   válida a primeira forma que inclui o DICIONÁRIO DA LÍNGUA GALEGA,
	   cando inclui duas, e a forma única cando só inclui unha. Pois
	   cuidamos que os critérios da APLL e do dito dicionário coinciden
	   basicamente, dos exemplos citados anteriormente só dous diferen.
	   Seguindo o DICIONÁRIO demos como válidas as formas procisión e
	   profisión.
	3) No caso dos comezos de palabra ca-/cua- e ga-/gua-, seguindo o
	   critério implícito no DICIONÁRIO DA LÍNGUA GALEGA demos por
	   válidas as formas en -ca, -gua.
	4) A hora de conxugar os verbos, e cando a NOVA GRAMÁTICA considera
	   válidas duas formas, escollemos a que escreben en primeiro lugar,
	   ex. copio=cópio -> copio, agás no caso da 2ª persoa do plural do
	   presente de imperativo, onde escollemos a segunda fora, ex.
	   xogade=xogai -> xogai
	5) Caso de xurdir unha ambigüidade non contemplada nos pontos
	   anteriores escollemos a forma recomendada polo DICIONÁRIO DA LÍNGUA
	   GALEGA


5. License

                       NOTA IMPORTANTE

    This is an unofficial translation of the GNU General Public License into
galician. It was not published by the Free Software Foundation, and does not
legally state the distribution terms for software that uses the GNU GPL
--only the original English text of the GNU GPL does that. However, we hope
that this translation will help galician speakers understand the GNU GPL
better. 

   Esta traduçom da Licença Pública Geral da GNU ao galego nom é oficial.
Non foi publicada pola Fundaçom para o Software Livre (Free Software
Foundation), e non estabelece legalmente os termos de distribuiçom do
software que usa a licença pública geral da GNU. De feito só a  versom
original, em inglês, é legal. Porém esperamos que esta traduçom ajude aos
galegofalantes e entender mellor a LPG da GNU. 



LICENÇA PÚBLICA GERAL DA GNU

Copyright (C) 1989, 1991 Free Software Foundation, Inc. 59 Temple Place -
Suite 330, Boston, MA  02111-1307, USA 

Everyone is permitted to copy and distribute verbatim copies of this license
document, but changing it is not allowed. 


Limiar

  As licenças da maioria do software estám desenhadas para cercear a
liberdade de comparti-lo e modifica-lo. Pola contra, a Licença Pública Geral
da GNU pretende garantir a liberdade de compartir e modificar o software
livre-- assegurando-se de que o software é livre para quaisquer usuários.
Esta Licença Pública Geral aplica-se a maioria do software da Fundaçom para
o Software Livre (FSF) e a qualquer programa cujo autor estea de acordo em
utiliza-lo. (Outro software da Fundaçom para o Software Livre é coberto pola
Licença Pública Geral de Livrarias da GNU). Você tamém pode utiliza-a para
os seus programas. 

  Quando falamos de software livre referimo-nos a liberdade e nom ao preço.
As nossas Licenças Públicas Gerais estám desenhadas para garantir que você
tem a liberdade de distribuir copias de software livre (e cobrar por esse
serviço, se o desejar), que recebe o código fonte ou que pode obte-lo se o
quer, que pode modificar o software ou empregar partes del em novos
programas livres; e que sabe que pode fazer tais cousas. 

  Para proteger os seus direitos cumpre fazermos algumhas restriçons que
proíbem a todas as persoas negar-lhe esses direitos ou solicitar-lhe a sua
renuncia a eles. Estas restriçons implicam certas responsabilidades para os
que distribuam copias ou as modifiquem. 

  Por exemplo, se distribui copias dum programa ao abeiro da licença, grátis
ou por umha taxa determinada, deve dar aos recipiendários todos os direitos
que você tem. Deve se assegurar de que eles recebam ou podam obter o código
fonte, assi como esta informaçom para que conheçam os seus direitos. 

  A protecçom dos seus direitos envolve dous passos: (1) copyright do
software, e (2) licença que da permissom legal para a cópia, distribuiçom
e/ou modificaçom do software. 

  Ademais, para a protecçom dos autores e a nossa própria, queremos
assegurar-nos de que todos entendam que nom hai garantias para este software
livre. Caso o software seja modificado por alguém e distribuído, queremos
que os usuários saibam que o que eles tenhem nom é o original, de jeito que
qualquer problema introduzido por outros nom se reflicta na reputaçom do
autor original. 

  Finalmente, qualquer programa de livre distribuiçom é ameaçado
constantemente polas patentes de software. Desejamos evitar o perigo de que
redistribuidores dum programa livre obtenham patentes individualmente,
tornando-se os seus donos efectivos. Para evitar isso, deixamos claro que
qualquer solicitaçom de patente deve ser feita permitindo o uso por qualquer
indivíduo, ou nom ser feita. 

  Seguem-se abaixo os termos e condiçons precisas para a cópia, distribuiçom
e modificaçom. 
  

TERMOS E CONDIÇONS PARA A CÓPIA, DISTRIBUIÇOM E MODIFICAÇOM

  0. Esta licença aplica-se a qualquer programa ou outro trabalho que
contenha um aviso colocado polo detentor dos direitos autorais dizendo que
este poderá ser distribuído nas condiçons da Licença Pública Geral. Em
adiante  o "Programa" refere-se a qualquer software ou trabalho,  e "um
trabalho baseado no Programa" significa tanto o Programa em si como
quaisquer trabalhos derivados del de acordo coa lei de direitos autorais,
quer dizer, um trabalho que contenha o Programa ou umha parte deste, na sua
forma original ou com modificaçons ou traduzido para outra língua (a
traduçom está incluída sem limitaçons no termo "modificaçom"). 

As actividades distintas da cópia, distribuiçom e modificaçom nom estám
cobertas por esta Licença, estando fora do seu escopo. O acto de executar o
Programa nom está restringido e a saída do Programa é coberta somente caso
do seu conteúdo conter trabalhos baseados no Programa (independentemente de
terem sido gerados pela execução do Programa). A exactitude do anterior
depende do que fai o Programa. 

  1. Pode copiar e distribuir copias inalteradas do código fonte, tal e como
as recebeu, em qualquer meio, sempre e quando inclua em cada cópia dum jeito
notório um aviso adequado sobre os copyrights e a falta de garantias;
mantenha intactos  todos os avisos que se referem à Licença Pública Geral e
à ausência de garantias; e que forneça a qualquer recipéndario umha cópia
desta licença junto co Programa. 

Pode cobrar umha taxa polo acto físico de transferência ou gravaçom de
cópias, e pode, se o considera oportuno, oferecer garantia e suporte por um
preço. 

  2. Pode modificar a sua cópia ou cópias do Programa ou qualquer parte del,
elaborando assi um trabalho baseado no Programa, e copiar e distribuir tais
modificaçons sob os termos da secçom 1 precedente, supondo que cumprem tamém
as seguintes condiçons: 

     a) Deve incluir umha nota em destaque em cada um dos arquivos
	   modificados avisando de que modificou o arquivo e da data de
	   qualquer alteraçom. 
     b) Deve procurar que qualquer trabalho que distribua ou publique, que
	   em todo o em parte contenha a ou seja derivado de o Programa, seja 
	   licenciado como um todo sem custe algum para terceiras partes sob os
	   termos desta Licença. 
     c) Caso o programa modificado leia normalmente comandos 
	   interactivamente na sua execuçom, deve  assegurar-se que ao começar
	   a execuçom no modo interactivo, do jeito mais comum,  apresente um
	   aviso apropriado coa informaçom de copyright e de ausência de
	   garantia (ou bem que você oferece garantia), dizendo que os usuários
	   podem redistribuir o programa sob estas condiçons, e indicando ao
	   usuário como ver umha copia desta Licença. (Excepçom: caso do
	   Programa ser interactivo mas nom amostrar normalmente um aviso tal
	   como o descrito, o seu trabalho baseado no Programa nom tem porque
	   apresentar tal aviso.)

Estes requisitos aplicam-se ao trabalho modificado como um todo. Caso
algumha secçom claramente identificável desse trabalho nom seja derivado do
Programa, podendo ser considerada razoavelmente como parte independente,
entom esta Licença e os seus Termos nom som de aplicaçom nessa secçom se
distribuída separadamente. Mais se distribuir tal secçom como parte dum
todo, baseado no Programa, a distribuiçom desse todo deve cumprir os termos
desta Licença, cujas permissons extendem-se ao trabalho como um todo, e nom
a cada umha das partes independentemente de quem os tenha desenvolvido.

Portanto, nom é a intençom desta secçom reclamar direitos ou contestar os
seus direitos sobre um trabalho desenvolvido inteiramente por você; a sua
intençom é exercer o direito de controlar a distribuiçom de trabalhos
derivados ou colectivos baseados no Programa. 

Adicionalmente, a mera adiçom de outro trabalho, nom baseado no Programa, 
ao Programa (ou a um trabalho baseado no Programa) num meio de armazenamento
ou distribuiçom nom obriga a utilizaçom desta Licença para o outro trabalho.


  3. Pode copiar e distribuir o Programa (ou um trabalho baseado nel, sob a
secçom 2) na forma de código objecto ou executável de acordo cos termos das
secçons 1 e 2 precedentes, sempre que ademais cumpra umha das seguintes
condiçons: 

  a) O acompanhe co código fonte completo correspondente, em formato legível
     para um computador,  sob os termos das secçons 1 e 2 precedentes, e num
	meio de uso normal para o intercámbio de software; ou bem, 
  b) O acompanhe com umha oferta escrita, válida durante polo menos 3 anos,
     de fornecer a terceiros, por um custo nom superior ao custo do meio
	físico de armazenamento, umha copia completa do código fonte
	correspondente, em formato legível para um computador. Copia que se
	distribuirá sob os termos das secçons 1 e 2 precedentes, e num meio de
	uso normal para o intercámbio de software; ou bem 
  c) O acompanhe coa mesma informaçom recebeu em relaçom à oferta da
     distribuiçom do código fonte correspondente. (Esta alternativa 
	permite-se unicamente para distribuiçons nom comerciais, e só se você
	recebeu o programa em forma de código objecto ou executável  com um
	oferecimento deste tipo, de acordo coa subsecçom b anterior.) 

O código fonte dum trabalho é a forma mais ajeitada para modifica-lo.  Para
um trabalho executável, o código fonte completo significa todas as fontes de
todos o módulos que contém, mais qualquer ficheiro de definiçom de
interfaces associadas, mais os scripts utilizados no controlo
da compilaçom e instalaçom do executável.  Porém, como umha excepçom
especial, o código fonte distribuído nom necessita incluir componentes
distribuídos normalmente (bem em forma binária ou em fonte) com os
componentes principais (compilador, núcleo, etc) do sistema operativo no cal
corra o executável,  a nom ser que tal componente acompanhe ao executável. 

Caso que a distribuiçom do executável ou código objecto se faga ofertando
acesso para copiar num determinado lugar, entom a oferta dum acesso
equivalente para copiar o código fonte no mesmo lugar considerara-se como
distribuiçom do código fonte, mesmo que terceiros nom esteam obrigados a
copiarem as fontes junto co código objecto. 

  4. Nom está permitida a copia, modificaçom, sublicenciamento ou
distribuiçom do Programa excepto sob as condiçons expressamente indicadas
nesta Licença. Qualquer tentativa diferente de copia, modificaçom,
sublicenciamento o distribuiçom está proibida, e cancelará automaticamente
os seus direitos sob esta Licença. Porém, os terceiros que receberam copias,
ou direitos de você sob esta Licença, nom verám os seus direitos cancelados,
entanto permaneçam dentro das cláusulas desta Licença. 

  5. Nom está obrigado a aceitar esta Licença, já que nom a assinou. Agora
bem, nom hai nada mais que garanta a sua permissom para modificar o
distribuir o Programa ou os trabalhos derivados del. Tais acçons estám
proibidas legalmente se nom aceita esta Licença. Portanto, ao modificar ou
distribuir o Programa (ou qualquer trabalho baseado nel), aceita tacitamente
a Licença, com todos os seus termos e condiçons para a cópia, distribuiçom e
modificaçom do Programa e dos trabalhos baseados nel. 

  6. Cada vez que redistribui o Programa (ou qualquer trabalho baseado nel),
o recipiendário recebe automaticamente umha licença, do detentor original
dos direitos, para copiar, distribuir ou modificar o Programa sujeita a
estes termos e condiçons. Você nom pode impor restriçons adicionais no
exercício dos direitos do recipiendário, aqui garantidos. Você nom está
obrigado a fazer cumprir esta Licença a terceiras partes.

  7. Se como consequência dumha sentença judicial, umha alegaçom de violaçom
de patente ou qualquer outra razom (nom limitada a questons de patentes),
imponhem-se-lhe condiçons (por ordem judicial, acordo ou doutro jeito
qualquer) contraditórias coas condiçons desta Licença, tais condiçons nom o
escusam do cumprimento dos termos desta Licença. Se  nom pode distribuir o
Programa de jeito que cumpra simultaneamente as obrigaçons que impom está
Licença mais outras obrigaçons pertinentes, entom e como consequência nom
poderá distribuir o programa em absoluto. Por exemplo, se umha patente nom
permite a redistribuiçom gratuita (sem pagar royalties) do Programa por
todos aqueles que recebem copias directa ou indirectamente de você, entom o
único jeito de satisfazer tanto a dita patente como esta Licença será
abster-se de distribuir o Programa. 

Se algumha porçom desta secçom é considerada inválida sob qualquer
circunstáncia particular, o resto da secçom será de aplicaçom, e a secçom
completa será de aplicaçom noutras circunstáncias. 

O propósito desta secçom nom é induzir-lhe a infringir nengumha patente, ou
outras reclamaçons de direitos de propriedade, nem mesmo negar a validade de
tais reclamaçons.  O único propósito desta secçom é proteger a integridade
do sistema de distribuiçom do software livre, implementado mediante a
prática de licenças públicas. Muita gente tem feito generosas contribuiçons
à ampla variedade de software distribuído mediante este sistema,  confiando
numha aplicaçom consistente do mesmo; é questom d@ autor@/doador@ decidir se
deseja distribuir software mediante qualquer outro sistema, a ninguém mais
cabe tal decisom. 

Com esta secçom pretende-se deixar bem claro o que se pensa é umha
consequência do resto desta Licença. 

  8. Se a distribuiçom e/ou o uso do Programa tivera restriçons em alguns
países bem por patentes ou por copyrights sobre interfaces, o detentor
original do copyright do Programa sob esta Licença pode engadir umha
limitaçom geográfica explicita excluindo tais países da distribuiçom, de
maneira que só se permita a distribuiçom nos países nom excluídos. Nesse
caso, esta Licença incorpora esta limitaçom, como se estivera escrita no
corpo desta Licença. 

  9. A Fundaçom para o Software Livre (Free Software Foundation) pode
publicar versons revisadas e/ou novas versons da Licencia Pública Geral de
tempos em tempos. Estas novas versons manterám o espirito da presente
versom, podendo variar em detalhes para adaptar-se as novas situaçons. 

A cada versom dá-se-lhe um número distinto, que serve para distiguí-las. 
Caso o Programa especifique o número de versom desta Licença que se lhe
aplica e "qualquer verson posterior", tem a opçom de seguir os termos e
condiçons dessa versom ou de qualquer outra versom posterior publicada pola
Fundaçom para o Software Livre (FSF).  Se o Programa nom especifica  umha
versom desta Licença, pode escolher qualquer versom das publicadas pola
Fundaçom para o Software Livre (FSF). 

  10. Se deseja incorporar partes do Programa noutros programas livres cujas
condiçons de  distribuiçom som diferentes, escreva ao autor para lhe
solicitar permissom. Se o software tem copyriht da Fundaçom para o Software
Livre (FSF), escreva-nos, as vezes fazemos excepçons. A nossa decisom estará
guiada por dous objectivos: preservar o status livre de todos os derivados
do nosso software livre, e promover a compartiçom e a reutilizaçom do
software em geral. 
  

AUSÊNCIA DE GARANTIAS 

  11. JÁ QUE O PROGRAMA É LICENCIADO SEM CARGAS, NOM HAI GARANTIAS PARA O
PROGRAMA, ATÉ O PERMITIDO POLAS LEIS APLICÁVEIS. FORA QUE SE ESTABELEÇA
DOUTRA FORMA POR ESCRITO, OS DETENTORES DO COPYRIGHT E/OU TERCEIROS FORNECEM
O PROGRAMA "COMO ESTÁ", SEM GARANTIAS DE QUALQUER NATUREZA, EXPRESSAS OU
IMPLÍCITAS, INCLUINDO, MAIS SEM LIMITAR-SE A, AS GARANTIAS IMPLÍCITAS
COMERCIAIS E DE UTILIDADE PARA UM PROPÓSITO PARTICULAR. A QUALIDADE E
EFICIÊNCIA SOM DE RISCO EXCLUSIVO DO USUÁRIO, CORRENDO POLA SUA CONTA OS
CUSTOS NECESSÁRIOS PARA A REPARAÇOM E A CORRECÇOM, CASO O PROGRAMA RESULTAR
DEFEITUOSO.

  12. EM NENGUM CASO, A MENOS QUE REQUERIDO POR LEI OU ACORDADO POR ESCRITO,
NENGUM DETENTOR DO COPYRIGHT, OU TERCEIRO QUE TENHA MODIFICADO E/OU
DISTRIBUÍDO O PROGRAMA,  SERÁ RESPONSÁVEL PERANTE VOCÊ DE DANOS OU
PREJUÍZOS, INCLUINDO QUALQUER DANO GERAL, ESPECIAL, INCIDENTAL OU INDIRECTO,
PROVENIENTES DO USO OU DA INCAPACIDADE DE USO DO PROGRAMA (INCLUINDO, MAS
SEM LIMITAR-SE,  A PERDA DE DADOS, A PRODUÇOM DE DADOS ERRÓNEOS, AS PERDAS
SOFRIDAS POR VOCÊ OU UM TERCEIRO, OU UMHA INCAPACIDADE DO PROGRAMA PARA
OPERAR COM OUTROS PROGRAMAS), MESMO SE O DETENTOR DO COPYRIGHT OU UM
TERCEIRO FORAM AVISADOS DA POSSIBILIDADE DE TAIS DANOS. 
  

FIM DA LICENÇA

  Como aplicar estes termos ao seus novos programas

  Se desenvolve um novo programa, e quer que tenha a máxima utilidade
possível para o público, a melhor forma de consegui-lo é torna-lo software
livre, para que qualquer poda distribui-lo e modifica-lo sob estes termos. 

  Para faze-lo, inclua os seguintes avisos no programa. Resulta mais seguro
os anexar no inicio de cada ficheiro fonte, para assi salientar claramente a
falta de garantias; e cada arquivo deve incluir ao menos umha linha co
copyright e umha indicaçom de onde se pode achar o aviso completo. 

	<umha linha para dar o nome do programa e umha breve descriçom do que
	 fai.> Copyright (C) 19yy  nome do autor

  (1) This program is free software; you can redistribute it and/or modify
      it under the terms of the GNU General Public License as published by
	 the Free Software Foundation; either version 2 of the License, or (at
	 your option) any later version.

	This program is distributed in the hope that it will be useful, but 
	WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	General Public License for more details.

	You should have received a copy of the GNU General Public License along
	with this program; if not, write to the Free Software Foundation, Inc.,
	59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Engada tamém informaçom de como contactar com você por correio electrónico e
convencional. 

Caso do programa seja interactivo, apresente na sua saída um breve aviso
como o seguinte ao iniciar-se em modo interactivo: 

  (2) Gnomovision version 69, Copyright (C) 19yy name of author Gnomovision
	 comes with ABSOLUTELY NO WARRANTY; for details type `show w'.  This is
	 free software, and you are welcome to redistribute it under certain
	 conditions; type `show c' for details.

Os comandos hipotéticos `show w' e `show c' deveriam amostrar as partes
apropriadas da Licença Pública Geral da GNU.  Evidentemente os comandos
podem receber outros nomes distintos de `show w' e `show c';  mesmo podem
ser accionados clicando o rato ou mediante menus --o que melhor se ajeite ao
seu programa.

Caso que fosse necessário, deveria conseguir que o seu empregador (se
trabalha como programador por conta alheia) ou o organismo para o que
trabalha, assina-se umha renuncia ao copyright do programa. Eis um exemplo,
modifique os nomes: 

Pola presente Yoyodyne, Inc. renuncia a todo interesse no copyright do
programa `Gnomovision' (que fai o que seja) escrito por James Hacker.

assinatura de Ty Coon, 1 April 1989
Ty Coon, Presidente Chefe

Esta Licença Pública Geral nom permite incorporar o seu programa em
programas proprietários. Se o seu é umha livraria de subrotinas, pode
considerar mais útil permitir enlaçar aplicaçons proprietárias coa livraria.
Nesse caso o resulta mais ajeitado utilizar a Licença Pública Geral para
Livrarias da GNU. 
  

Notas do Tradutor

(1)  Dado que a Licença Pública Geral da GNU oficial é a versom em inglês,
se deseja pôr um programa ao abeiro de tal licença deverá acompanhar a
versom oficial da dita Licença, e parece lógico entom incluir os avisos em
inglês. A traduçom do aviso seria a seguinte: 

	Este programa é software livre; pode redistribui-lo e/ou modifica-lo
	sob os termos da Licença Pública Geral da GNU publicada pola Fundaçom
	para o Software Livre; bem a versom 2 da Licença, ou (como deseje)
	qualquer versom posterior.

	Distribui-se este programa esperando que resulte útil, mas SEM NENGUMHA
	GARANTIA; mesmo sem a garantia implícita de COMERCIABILIDADE ou de
	UTILIDADE PARA UM PROPÓSITO PARTICULAR. Veja-se a Licença Pública Geral
	da GNU para maiores detalhes.

	Deve ter recebido umha copia do Licença Pública Geral da GNU com este
	programa;  caso contrario, escreva à Fundaçom para o Software  Livre,
	Inc. 59 Temple Place - Suite 330, Boston, MA  02111-1307, EUA.

 (2) Gnomovision versom 69, Copyright (C) 19yy nome do autor Gnomovision vem
     sem NENGUMHA GARANTIA; para mais detalhes digite `amostre w'.  Este é
	software livre, e anima-se-lhe a redistribui-lo sob certas condiçons;
	digite `mostre c' para mais detalhes.

-------------- 

Traduzido por Ramón Flores <fa2ramon@usc.es>

Mais informaçom sobre o software livre e a GNU en
http://members.tripod.com.br/ramonflores/GNU/limiar.html


6. ***leme.txt ***


Dicionário galego para o corrector ortográfico MySpell, que é o corrector usado
no OpenOffice.org.

1. Licenza
2. Contido
3. Instalación
4. Limitacións
5. Consellos

==========
1. Licenza
==========

Este é un dicionário de galego (mínimos) para o corrector ortográfico MySpell, e
basea-se no dicionário para Ispell da autoria de André Ventas e Ramón Flores
(http://ispell-gl.sourceforge.net), que se distribui ao abeiro da Licenza
Pública Xeral da GNU, GNU GPL, versión 2. As modificacións feitas para adaptar o
dicionário ao MySpell son da autoria de Ramón Flores, estando o dicionário
modificado coberto tamén pola Licenza Públic Xeral da GNU, GNU GPL, versión 2.


==========
2. Contido
==========

Este pacote consta dos seguintes ficheiros:

   gl_ES.dic,  é unha lista de palabras raices.
   gl_ES.aff,  consta das regras que permiten derivar palabras a partir da  
               raices.
   leme.txt,   este ficheiro.
   readme.txt, inclui información sobre o dicionário en inglés.
   COPYING     texto da GNU GPL en inglés.
   LICENZA.txt tradución da GNU GPL ao galego (AGAL).
   normas.txt  inclui información sobre a normativa de mínimos.
	
================================
3. Instalación no OpenOffice.org
================================

   a. Feche todas as xanelas do OpenOffice.org incluindo o Quickstarter antes de
      instalar e editar o arquivo dictionary.lst.
   b. Copie os arquivos gl_ES.dic e gl_ES.aff no directório,
        OpenOffice.org/user/wordbook/    (Para OpenOffice.org 1.0.0.)
        OpenOffice.org/share/dict/ooo/   (Para a versión 1.0.1 ou superior)
   c. (Vexa a nota) Abra o arquivo dictionary.lst, que está no mesmo directório,
      usando calquer editor de texto (WordPad, Kedit...) e engada  a seguinte 
      liña:
               DICT gl ES gl_ES
      
      Esta liña di-lle a OpenOffice.org que asocie os arquivos gl_ES.aff e 
      gl_ES.dic ao locale, gl ES, Galego (España).

      Os campos que aparecen nesta liña son os seguintes:

      Campo 1: Tipo de entrada, "DICT" significa loxicamente dicionário.
      Campo 2: Código da língua do Locale "gl" ou "en" ou "pt" ... 
      Campo 3: Código do pais do Locale "ES" ou "GB" ou "PT" ... 
      Campo 4: Nome raiz do dicionário "english" ou "en_US" ou ... (sen incluir
               a extensión .aff ou .dic)

   d. Agora inicie o OpenOffice.org, e vaia a:
		Ferramentas->Opcións->Configuración de Língua->Lingüística 
      prema "Editar" e escolla no menu "galego". Marque entón no cadro que
      aparece baixo de Ortografia.
   
   e. Para verificar a ortografia dun documento usando este dicionário cumpre
      ir a:   
		Ferramentas->Opcións->Configuración de Língua->Línguas
      e escoller como língua padrón para documentos o galego.		

NOTA IMPORTANTE
 A dia de hoxe, 30/09/02, o OpenOffice ainda non recoñece o galego, asi que
cumpre engana-lo asociando o dicionário galego con outro locale que non vaia
usar, por exemplo o africaan af_ZA Afrikaans(Sul Africa), ou o albanés sq_AL
Albanés(Albánia). Como se fai isto?, moi sinxelo, os pontos a. e b. son
iguais, variando lixeiramente o c., o d. e mais o e.:
 
   d. Abra o arquivo dictionary.lst, que está no mesmo directório, usando
      calquer editor de texto e engada a seguinte liña:
          DICT af ZA gl_ES           ( ou   DICT sq_AL)
 
   e. Agora inicie o OpenOffice.org, e vaia a:
		Ferramentas->Opcións->Configuración de Língua->Lingüística 
      prema "Editar" e escolla no menu "afrikaans" (ou "albanian"). Marque entón
      no cadro que aparece baixo de Ortografia.
   
   f. Para verificar a ortografia dun documento usando este dicionário cumpre
      ir a:   
		Ferramentas->Opcións->Configuración de Língua->Línguas
      e escoller como língua padrón para documentos o afrikaans (albanian).		

   


==============
4. LIMITACIÓNS 
==============

  1.  Esta é a versión 0.3 e ainda dista de ser completo, contando unicamente
con 15051 palabras raices no galician.dic. Contodo como o ficheiro de afixos, 
galician.aff, é bastante completo, o total de palabras recoñecidas é de 214328. 
Asi por exemplo da palabra raiz "amor" derivan-se 53 formas verbais distintas, 
e ademais "amador, amadora, amadores, amadoras, amante, amantes, amábel e amábeis.

  2. Os pronomes, ou contraccións de pronomes, átonos enclíticos unen-se ao verbo
mediante o trazo. O MySpell con este dicionário recoñece a maioria das formas
verbais con pronomes enclíticos, mais non da recoñecido como válidas as 1ª e 2ª
persoas do plural de caisquer tempos seguidos dos pronomes átonos -lo(s) e
-la(s), e tampouco as 1ª persoas do plural seguidas do pronome -nos.

    Asi non recoñece: collemo-lo, collede-las, colleste-los, collíamo-la,
		      colleremo-nos ...
         si recoñece: collé-lo, collin-na, colleche-mo, collerán-cho,
	              collemos-lle, collestes-nos, coller-se, collias-lles,
		      colleran-vos, colleres-lle-la ...

Da como válidas falsamente as formas collé, andá ... que son necesárias
para dar por válidas (correctamente): collé-lo, andá-los, etc. Consideramos
que as formas collé, andá, etc dificilmente poden ser escritas por erro.



============
5. CONSELLOS
============

O MySpell non sempre apresenta as suxeréncias máis acaídas, asi se unha palabra
non é recoñecida como correcta é conveniente facer o seguinte:

	1) Caso de ter escrito unha forma rematada en "-ción" ou "-sión"
	   troca-la pola rematada en "-zón" ou "-són"
	2) Caso de ter escrito unha forma rematada en "-zón" ou "-són" troca-la
	   pola rematada en "-ción" ou "-sión"
        3) Se ha unha "b" troca-la por unha "v"
	4) Se ha unha "v" troca-la por unha "b"


7. Copying

		    GNU GENERAL PUBLIC LICENSE
		       Version 2, June 1991

 Copyright (C) 1989, 1991 Free Software Foundation, Inc.
                          675 Mass Ave, Cambridge, MA 02139, USA
 Everyone is permitted to copy and distribute verbatim copies
 of this license document, but changing it is not allowed.

			    Preamble

  The licenses for most software are designed to take away your
freedom to share and change it.  By contrast, the GNU General Public
License is intended to guarantee your freedom to share and change free
software--to make sure the software is free for all its users.  This
General Public License applies to most of the Free Software
Foundation's software and to any other program whose authors commit to
using it.  (Some other Free Software Foundation software is covered by
the GNU Library General Public License instead.)  You can apply it to
your programs, too.

  When we speak of free software, we are referring to freedom, not
price.  Our General Public Licenses are designed to make sure that you
have the freedom to distribute copies of free software (and charge for
this service if you wish), that you receive source code or can get it
if you want it, that you can change the software or use pieces of it
in new free programs; and that you know you can do these things.

  To protect your rights, we need to make restrictions that forbid
anyone to deny you these rights or to ask you to surrender the rights.
These restrictions translate to certain responsibilities for you if you
distribute copies of the software, or if you modify it.

  For example, if you distribute copies of such a program, whether
gratis or for a fee, you must give the recipients all the rights that
you have.  You must make sure that they, too, receive or can get the
source code.  And you must show them these terms so they know their
rights.

  We protect your rights with two steps: (1) copyright the software, and
(2) offer you this license which gives you legal permission to copy,
distribute and/or modify the software.

  Also, for each author's protection and ours, we want to make certain
that everyone understands that there is no warranty for this free
software.  If the software is modified by someone else and passed on, we
want its recipients to know that what they have is not the original, so
that any problems introduced by others will not reflect on the original
authors' reputations.

  Finally, any free program is threatened constantly by software
patents.  We wish to avoid the danger that redistributors of a free
program will individually obtain patent licenses, in effect making the
program proprietary.  To prevent this, we have made it clear that any
patent must be licensed for everyone's free use or not licensed at all.

  The precise terms and conditions for copying, distribution and
modification follow.

		    GNU GENERAL PUBLIC LICENSE
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

  0. This License applies to any program or other work which contains
a notice placed by the copyright holder saying it may be distributed
under the terms of this General Public License.  The "Program", below,
refers to any such program or work, and a "work based on the Program"
means either the Program or any derivative work under copyright law:
that is to say, a work containing the Program or a portion of it,
either verbatim or with modifications and/or translated into another
language.  (Hereinafter, translation is included without limitation in
the term "modification".)  Each licensee is addressed as "you".

Activities other than copying, distribution and modification are not
covered by this License; they are outside its scope.  The act of
running the Program is not restricted, and the output from the Program
is covered only if its contents constitute a work based on the
Program (independent of having been made by running the Program).
Whether that is true depends on what the Program does.

  1. You may copy and distribute verbatim copies of the Program's
source code as you receive it, in any medium, provided that you
conspicuously and appropriately publish on each copy an appropriate
copyright notice and disclaimer of warranty; keep intact all the
notices that refer to this License and to the absence of any warranty;
and give any other recipients of the Program a copy of this License
along with the Program.

You may charge a fee for the physical act of transferring a copy, and
you may at your option offer warranty protection in exchange for a fee.

  2. You may modify your copy or copies of the Program or any portion
of it, thus forming a work based on the Program, and copy and
distribute such modifications or work under the terms of Section 1
above, provided that you also meet all of these conditions:

    a) You must cause the modified files to carry prominent notices
    stating that you changed the files and the date of any change.

    b) You must cause any work that you distribute or publish, that in
    whole or in part contains or is derived from the Program or any
    part thereof, to be licensed as a whole at no charge to all third
    parties under the terms of this License.

    c) If the modified program normally reads commands interactively
    when run, you must cause it, when started running for such
    interactive use in the most ordinary way, to print or display an
    announcement including an appropriate copyright notice and a
    notice that there is no warranty (or else, saying that you provide
    a warranty) and that users may redistribute the program under
    these conditions, and telling the user how to view a copy of this
    License.  (Exception: if the Program itself is interactive but
    does not normally print such an announcement, your work based on
    the Program is not required to print an announcement.)

These requirements apply to the modified work as a whole.  If
identifiable sections of that work are not derived from the Program,
and can be reasonably considered independent and separate works in
themselves, then this License, and its terms, do not apply to those
sections when you distribute them as separate works.  But when you
distribute the same sections as part of a whole which is a work based
on the Program, the distribution of the whole must be on the terms of
this License, whose permissions for other licensees extend to the
entire whole, and thus to each and every part regardless of who wrote it.

Thus, it is not the intent of this section to claim rights or contest
your rights to work written entirely by you; rather, the intent is to
exercise the right to control the distribution of derivative or
collective works based on the Program.

In addition, mere aggregation of another work not based on the Program
with the Program (or with a work based on the Program) on a volume of
a storage or distribution medium does not bring the other work under
the scope of this License.

  3. You may copy and distribute the Program (or a work based on it,
under Section 2) in object code or executable form under the terms of
Sections 1 and 2 above provided that you also do one of the following:

    a) Accompany it with the complete corresponding machine-readable
    source code, which must be distributed under the terms of Sections
    1 and 2 above on a medium customarily used for software interchange; or,

    b) Accompany it with a written offer, valid for at least three
    years, to give any third party, for a charge no more than your
    cost of physically performing source distribution, a complete
    machine-readable copy of the corresponding source code, to be
    distributed under the terms of Sections 1 and 2 above on a medium
    customarily used for software interchange; or,

    c) Accompany it with the information you received as to the offer
    to distribute corresponding source code.  (This alternative is
    allowed only for noncommercial distribution and only if you
    received the program in object code or executable form with such
    an offer, in accord with Subsection b above.)

The source code for a work means the preferred form of the work for
making modifications to it.  For an executable work, complete source
code means all the source code for all modules it contains, plus any
associated interface definition files, plus the scripts used to
control compilation and installation of the executable.  However, as a
special exception, the source code distributed need not include
anything that is normally distributed (in either source or binary
form) with the major components (compiler, kernel, and so on) of the
operating system on which the executable runs, unless that component
itself accompanies the executable.

If distribution of executable or object code is made by offering
access to copy from a designated place, then offering equivalent
access to copy the source code from the same place counts as
distribution of the source code, even though third parties are not
compelled to copy the source along with the object code.

  4. You may not copy, modify, sublicense, or distribute the Program
except as expressly provided under this License.  Any attempt
otherwise to copy, modify, sublicense or distribute the Program is
void, and will automatically terminate your rights under this License.
However, parties who have received copies, or rights, from you under
this License will not have their licenses terminated so long as such
parties remain in full compliance.

  5. You are not required to accept this License, since you have not
signed it.  However, nothing else grants you permission to modify or
distribute the Program or its derivative works.  These actions are
prohibited by law if you do not accept this License.  Therefore, by
modifying or distributing the Program (or any work based on the
Program), you indicate your acceptance of this License to do so, and
all its terms and conditions for copying, distributing or modifying
the Program or works based on it.

  6. Each time you redistribute the Program (or any work based on the
Program), the recipient automatically receives a license from the
original licensor to copy, distribute or modify the Program subject to
these terms and conditions.  You may not impose any further
restrictions on the recipients' exercise of the rights granted herein.
You are not responsible for enforcing compliance by third parties to
this License.

  7. If, as a consequence of a court judgment or allegation of patent
infringement or for any other reason (not limited to patent issues),
conditions are imposed on you (whether by court order, agreement or
otherwise) that contradict the conditions of this License, they do not
excuse you from the conditions of this License.  If you cannot
distribute so as to satisfy simultaneously your obligations under this
License and any other pertinent obligations, then as a consequence you
may not distribute the Program at all.  For example, if a patent
license would not permit royalty-free redistribution of the Program by
all those who receive copies directly or indirectly through you, then
the only way you could satisfy both it and this License would be to
refrain entirely from distribution of the Program.

If any portion of this section is held invalid or unenforceable under
any particular circumstance, the balance of the section is intended to
apply and the section as a whole is intended to apply in other
circumstances.

It is not the purpose of this section to induce you to infringe any
patents or other property right claims or to contest validity of any
such claims; this section has the sole purpose of protecting the
integrity of the free software distribution system, which is
implemented by public license practices.  Many people have made
generous contributions to the wide range of software distributed
through that system in reliance on consistent application of that
system; it is up to the author/donor to decide if he or she is willing
to distribute software through any other system and a licensee cannot
impose that choice.

This section is intended to make thoroughly clear what is believed to
be a consequence of the rest of this License.

  8. If the distribution and/or use of the Program is restricted in
certain countries either by patents or by copyrighted interfaces, the
original copyright holder who places the Program under this License
may add an explicit geographical distribution limitation excluding
those countries, so that distribution is permitted only in or among
countries not thus excluded.  In such case, this License incorporates
the limitation as if written in the body of this License.

  9. The Free Software Foundation may publish revised and/or new versions
of the General Public License from time to time.  Such new versions will
be similar in spirit to the present version, but may differ in detail to
address new problems or concerns.

Each version is given a distinguishing version number.  If the Program
specifies a version number of this License which applies to it and "any
later version", you have the option of following the terms and conditions
either of that version or of any later version published by the Free
Software Foundation.  If the Program does not specify a version number of
this License, you may choose any version ever published by the Free Software
Foundation.

  10. If you wish to incorporate parts of the Program into other free
programs whose distribution conditions are different, write to the author
to ask for permission.  For software which is copyrighted by the Free
Software Foundation, write to the Free Software Foundation; we sometimes
make exceptions for this.  Our decision will be guided by the two goals
of preserving the free status of all derivatives of our free software and
of promoting the sharing and reuse of software generally.

			    NO WARRANTY

  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE
PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
REPAIR OR CORRECTION.

  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING
OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED
TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY
YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER
PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGES.

		     END OF TERMS AND CONDITIONS

	Appendix: How to Apply These Terms to Your New Programs

  If you develop a new program, and you want it to be of the greatest
possible use to the public, the best way to achieve this is to make it
free software which everyone can redistribute and change under these terms.

  To do so, attach the following notices to the program.  It is safest
to attach them to the start of each source file to most effectively
convey the exclusion of warranty; and each file should have at least
the "copyright" line and a pointer to where the full notice is found.

    <one line to give the program's name and a brief idea of what it does.>
    Copyright (C) 19yy  <name of author>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Also add information on how to contact you by electronic and paper mail.

If the program is interactive, make it output a short notice like this
when it starts in an interactive mode:

    Gnomovision version 69, Copyright (C) 19yy name of author
    Gnomovision comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
    This is free software, and you are welcome to redistribute it
    under certain conditions; type `show c' for details.

The hypothetical commands `show w' and `show c' should show the appropriate
parts of the General Public License.  Of course, the commands you use may
be called something other than `show w' and `show c'; they could even be
mouse-clicks or menu items--whatever suits your program.

You should also get your employer (if you work as a programmer) or your
school, if any, to sign a "copyright disclaimer" for the program, if
necessary.  Here is a sample; alter the names:

  Yoyodyne, Inc., hereby disclaims all copyright interest in the program
  `Gnomovision' (which makes passes at compilers) written by James Hacker.

  <signature of Ty Coon>, 1 April 1989
  Ty Coon, President of Vice

This General Public License does not permit incorporating your program into
proprietary programs.  If your program is a subroutine library, you may
consider it more useful to permit linking proprietary applications with the
library.  If this is what you want to do, use the GNU Library General
Public License instead of this License.
