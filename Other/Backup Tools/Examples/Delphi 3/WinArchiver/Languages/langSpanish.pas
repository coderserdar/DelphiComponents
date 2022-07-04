unit langSpanish;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, 'Selecciona "Nuevo" o "Abrir" un archivo' );
  AddStr(   2, 'Aceptar' );
  AddStr(   3, 'Cancelar' );
  AddStr(   4, '&Ayuda' );
  // unit fmAboutBox
  AddStr( 500, 'Acerca' );
  AddStr( 501, 'por Morgan Martinet (C)1998' );
  AddStr( 502, 'Componentes Freeware.' );
  AddStr( 503, 'Copyright (C) 1998 por NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'Libreria pasZLib:' );
  AddStr( 505, 'mmm@imaginet.fr or mmm@mcom.fr' );
  AddStr( 506, 'Implementación BlowFish por Greg Carter, CRYPTOCard' );
  AddStr( 507, 'Codigo SFX por Oliver Buschjost' );
  AddStr( 508, 'Sitio Web:' );
  // unit fmTiming
  AddStr( 600, 'Tiempo transcurrido:' );
  AddStr( 601, 'Tiempo restante:' );
  // unit fmMain
  AddStr( 700, 'Nuevo...' );
  AddStr( 701, 'Abrir...' );
  AddStr( 702, '&Agregar...' );
  AddStr( 703, '&Extraer...' );
  AddStr( 704, '&Borrar...' );
  AddStr( 705, '&Abortar' );
  AddStr( 706, 'Archivo nuevo' );
  AddStr( 707, 'Abrir archivo' );
  AddStr( 708, 'Agregar archivos...' );
  AddStr( 709, 'Extraer archivos...' );
  AddStr( 710, 'Borrar archivos...' );
  AddStr( 711, 'Archivos de datos (*.mmm)|*.mmm|Archivos SFX (*.exe)|*.exe|Todos (*.*)|*.*' );
  AddStr( 712, 'Archivos de datos (*.mmm)|*.mmm|Todos (*.*)|*.*' );
  AddStr( 713, 'Abrir un archivo existente' );
  AddStr( 714, 'Crear un archivo nuevo' );
  AddStr( 715, 'Abrir un segmento de un archivo' );
  AddStr( 718, '%d Archivo(s), %s' );
  AddStr( 720, 'El archivo "%s" ya existe' );
  AddStr( 721, '¿ Desea reinicializar este archivo ?' );
  AddStr( 722, '¿ Desea borrar este archivo ?' );
  AddStr( 723, '%.0n Byte' );
  AddStr( 724, '%.0n Bytes' );
  AddStr( 725, '%.0n Kb' );
  AddStr( 726, '%.0n Mb' );
  AddStr( 727, 'seleccionó %d archivo(s), %s' );
  AddStr( 729, 'No disponible' );
  AddStr( 730, 'Renombre el archivo actual a:' );
  AddStr( 731, '¡ No se puede renombrar el archivo a "%s" !' );
  AddStr( 732, 'Configuración SFX' );
  AddStr( 733, 'Creando un archivo auto-extraible' );
  AddStr( 734, 'Crear' );
  AddStr( 735, '¡ No se puede crear un archivo auto-extraible !' );
  AddStr( 736, 'Coloque comentario de archivo' );
  AddStr( 737, 'Comentario de archivo' );
  AddStr( 738, 'Operacion en curso. Espere a que termine o precione Abortar.' );
  AddStr( 739, 'Tiene un archivo ejecutandose. Terminelo antes e intentelo de nuevo.' );
  AddStr( 740, 'Primero debe arbrir o crear un archivo.' );
  AddStr( 741, 'No se encuentra programa asociado a %s' );
  AddStr( 742, 'Nombre' );
  AddStr( 743, 'Fecha' );
  AddStr( 744, 'Hora' );
  AddStr( 745, 'Tamaño' );
  AddStr( 746, 'Radio' );
  AddStr( 747, 'Comprimido' );
  AddStr( 748, '#Seg' );
  AddStr( 749, 'Ruta' );
  AddStr( 750, '&Archivo' );
  AddStr( 751, '&Acciones' );
  AddStr( 752, '&Opciones' );
  AddStr( 753, 'A&yuda' );
  AddStr( 754, 'Archivo &nuevo...' );
  AddStr( 755, '&Abrir archivo...' );
  AddStr( 756, 'Abrir s&egmento...' );
  AddStr( 757, '&Cerrar archivo' );
  AddStr( 758, '&Información...' );
  AddStr( 759, 'Ren&ombrar archivo' );
  AddStr( 760, '&Reiniciar archivo' );
  AddStr( 761, '&Borrar archivo' );
  AddStr( 762, '&Salir' );
  AddStr( 763, '&Ver...' );
  AddStr( 764, '&Selecionar todo' );
  AddStr( 765, '&Fabricar archivo .EXE' );
  AddStr( 766, 'Poner comentario de archivo...' );
  AddStr( 767, 'Configuración &SFX...' );
  AddStr( 769, '&Acerca...' );
  AddStr( 770, 'Crear archivo nuevo' );
  AddStr( 771, 'Abrir archivo existente' );
  AddStr( 772, 'Abrir un segmento de un archivo' );
  AddStr( 773, 'Cerrar el archivo' );
  AddStr( 774, 'Mostrar información sobre el achivo' );
  AddStr( 775, 'Renombrar archivo actual...' );
  AddStr( 776, 'Reiniciar el contenido del archivo' );
  AddStr( 777, 'Borrar archivo' );
  AddStr( 778, 'Salir de la aplicación' );
  AddStr( 781, 'Agregar archivos al paquete' );
  AddStr( 782, 'Extraer archivos del paquete' );
  AddStr( 783, 'Borrar archivos del paquete' );
  AddStr( 784, 'Ver archivos' );
  AddStr( 785, 'Seleccionar todos los archivos del paquete' );
  AddStr( 786, 'Fabricar archivo auto-extraible' );
  AddStr( 787, 'Comentar el paquete' );
  AddStr( 788, 'Cambiar la configuración' );
  AddStr( 789, 'Cambiar la configuración de la creación SFX' );
  AddStr( 790, 'Acerca de la aplicación' );
  AddStr( 798, '&Configuración...' );
  AddStr( 799, '%s Archivo' );
  AddStr( 800, 'Cerrar paquete...' );
  AddStr( 801, '&Deseleccionar' );
  AddStr( 802, '&Inverir selección' );
  AddStr( 803, 'Raiz' );
  AddStr( 804, 'Ver Arbol' );
  AddStr( 805, 'Iconos grandes' );
  AddStr( 806, 'Iconos pequeños' );
  AddStr( 807, 'Lista' );
  AddStr( 808, 'Detalle' );
  AddStr( 809, 'Expandir Todo' );
  AddStr( 810, 'Colapsar Todo' );
  AddStr( 811, 'Borrar lista de archivos' );
  AddStr( 812, 'Fabricar lista de archivos' );
  AddStr( 813, 'Ordenar lista de archivos' );
  AddStr( 814, '¡ El archivo %s no existe !' );
  AddStr( 815, 'Verificar &integridad' );
  AddStr( 816, 'Verificando la integridad el actual archivo' );
  AddStr( 817, '&Ver último resultado...' );
  AddStr( 818, 'Ver resultado de la ultima operación' );
  AddStr( 819, 'Instalar' );
  AddStr( 820, 'Extraer contenido y ejecutar la instalación' );
  AddStr( 821, '&Fuente...' );
  AddStr( 822, 'Or&denar' );
  AddStr( 823, '&Orden original' );
  AddStr( 824, 'Cambiar la actual fuente' );
  AddStr( 825, 'Seleccionar ordenamiento' );
{*}  AddStr( 826, 'F&ilters...' );
{*}  AddStr( 827, 'Lets you define filters to select files to be added' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, 'Agregar desde' );
  AddStr( 901, 'Nombre :' );
  AddStr( 902, 'Carpetas' );
  AddStr( 903, 'Incluir carpetas y contenidos' );
  AddStr( 904, 'Incluir ruta actual' );
  AddStr( 905, 'Incluir carpetas vacias' );
  AddStr( 906, 'Ruta de almacenaje :' );
  AddStr( 907, '¿ Encriptar archivos ?' );
  AddStr( 908, 'Nivel de Compresión:' );
  AddStr( 909, 'Agregar' );
  AddStr( 910, 'Ninguna'+#13+
               'Relacionada'+#13+
               'Relativa' );
  AddStr( 911, 'Maxima (lenta)'+#13+
               'Normal'+#13+
               'Rapida'+#13+
               'Muy Rapida'+#13+
               'Ninguna' );
  AddStr( 912, 'Agregar archivos bajados' );
  AddStr( 913, 'Agregar elementos' );
  AddStr( 914, 'Filtro :' );
  AddStr( 915, '¿ Agregar actual carpeta ?' );
{*}AddStr( 916, 'Filter files ?' );

  // unit fmConfiguration

  AddStr( 1000, 'Configuración' );
  AddStr( 1001, 'División en discos' );
  AddStr( 1002, 'Creación de Paquete' );
  AddStr( 1003, 'Opciones' );
  AddStr( 1004, 'División de paquete' );
  AddStr( 1005, 'Tamaño Maximo del segmento:' );
  AddStr( 1006, '720 Kb'+#13+
                '1,44 Mb'+#13+
                'Otro (Kb):' );
  AddStr( 1007, 'Comprimir archivo' );
  AddStr( 1008, 'Encriptar archivo' );
  AddStr( 1009, 'Archivo solido' );
  AddStr( 1010, 'Solo Lectura' );
  AddStr( 1011, 'Crear archivo SFX' );
  AddStr( 1014, 'Tamaño de Bloque' );
  AddStr( 1015, 'Espacio reservado' );
  AddStr( 1016, 'Kb' );
  AddStr( 1017, 'Idioma:' );
  AddStr( 1018, 'Automatico'+#13+
                'Ingles'+#13+
                'Frances'+#13+
                'Chino'+#13+
                'Portugues'+#13+
                'Aleman'+#13+
                'Italiano'+#13+
                'Ruso'+#13+
                'Español'+#13+
{*}             'Danish'+#13+
                'Dutch'+#13+
                'Czech'
                );
  AddStr( 1019, 'Ver carpetas vacias' );
  AddStr( 1020, 'Ver Arbol' );
  // unit fmCreateFolder
  AddStr( 1100, 'Carpeta actual:' );
  AddStr( 1101, 'Nombre:' );
  // unit fmDelete
  AddStr( 1200, 'Borrar' );
  AddStr( 1201, 'Archivos' );
  AddStr( 1202, '&Todos los archivos'+#13+
                'Archivos &Seleccionados'+#13+
                '&Archivos:' );
  // unit fmEnterCryptKey
  AddStr( 1300, 'Mensaje de Sistema' );
  AddStr( 1301, '¿ Ocultar Clave ?' );
  // unit fmExtract
  AddStr( 1400, 'Extraer' );
  AddStr( 1401, 'Extraer a:' );
  AddStr( 1402, 'Archivos' );
  AddStr( 1403, 'Archivos &seleccionados'+#13+
                '&Todos los archivos'+#13+
                '&Archivos:' );
  AddStr( 1404, '&Usar nombre de carpeta' );
  AddStr( 1405, 'Sobreescribir archivos existentes'+#13+
                'Omitir archivos existentes'+#13+
                'Actualizar sólo archivos nuevos'+#13+
                'Preguntar confirmación' +#13+
                'Restaurar sólo archivos existentes'+#13+
                'Actualizar sólo archivos existentes' );
  AddStr( 1406, 'Carpetas / Unidad' );
  AddStr( 1407, 'Nueva carpeta...' );
  // unit fmHelpOnSFX
  AddStr( 1500, 'Las siguientes combinaciones pueden ser usadas en la linea de commandos'+#13+
                'y "Ruta predeterminada de extracción" campos :' );
  AddStr( 1501, 'Reemplaza el directorio temporal'+#13+
                '(Regularmente ''c:\windows\temp'' o ''c:\win95\temp'' o ''c:\temp'')' );
  AddStr( 1502, 'Reemplaza el directorio de Windows'+#13+
                '(Regularmente ''c:\windows'' or ''c:\win95'')' );
  AddStr( 1503, 'Reemplaza el directorio de Sistema'+#13+
                '(Regularmente ''c:\windows\system'' o ''c:\win95\system'')' );
  AddStr( 1504, 'Reemplaza el directorio de Archivos de Programas'+#13+
                '( Regularmente ''C:\Program Files'' o ''Archivos de Programas'''+#13+
                '[Dependiendo del Idioma de Windows instalado] )' );
  AddStr( 1505, 'Reemplaza el directorio donde se extraeran los archivos'+#13+
                '(solo para "Linea de Comandos" o el campo de "Argumentos")' );
  AddStr( 1506, 'Ejemplo:' );
  AddStr( 1507, '<PF>MiCompañia\MiTrabajo' );
  // unit fmInformation
  AddStr( 1600, 'Ruta:' );
  AddStr( 1601, 'Nombre:' );
  AddStr( 1602, 'Tamaño:' );
  AddStr( 1603, 'Archivos:' );
  AddStr( 1604, 'Compresión:' );
  AddStr( 1605, 'Fecha/Hora:' );
  AddStr( 1606, 'Segmento:' );
  AddStr( 1607, 'Atributos' );
  AddStr( 1608, 'Encriptado' );
  AddStr( 1609, 'Comprimido' );
  AddStr( 1610, 'Solido' );
  AddStr( 1611, 'Solo Lectura' );
  AddStr( 1612, 'Segmento Final' );
  AddStr( 1613, 'Información' );
  // unit fmSFXComments
  AddStr( 1700, 'Comenarios' );
  AddStr( 1701, 'Mostrar comentarios cuando se abra el archivo SFX' );
  AddStr( 1702, 'Mostrar comentario despues de la extracción del archivo SFX' );
  AddStr( 1703, 'Borrar comentarios' );
  // unit fmSFXConfig
  AddStr( 1800, 'Configuración SFX' );
  AddStr( 1801, '¿ Executar archivo después de la extracción ?' );
  AddStr( 1802, '¿ El usuario escoge archivos a extraer ?' );
  AddStr( 1803, '¿ El usuaro escoge sobreescritura ?');
  AddStr( 1804, 'Etiqueta:' );
  AddStr( 1805, 'Linea de comando:' );
  AddStr( 1806, 'Argumentos:' );
  AddStr( 1807, 'Ruta de extracción predeterminada:' );
  AddStr( 1808, 'Sobreescritura:' );
  AddStr( 1809, 'Comentarios...' );
  AddStr( 1810, 'Preguntar confirmación'+#13+
                'Sobreescribir archivos existentes'+#13+
                'Omitir archivos existentes'+#13+
                'Sobrescribir sólo archivos nuevos'+#13+
                'Restaurar sólo archivos existentes'+#13+
                'Extraer sólo archivos existentes y sólo si son nuevos' );
  AddStr( 1811, '¿ El usuario podrá  no ejecutar el archivo ?' );
  // unit fmTextViewer
  AddStr( 1900, 'Ver: %s' );
  AddStr( 1901, '&Copiar al Porta papeles' );
  AddStr( 1902, '&Fuente' );
  // unit fmView
  AddStr( 2000, 'Ver : %s' );
  AddStr( 2001, 'Usar' );
  AddStr( 2002, 'Ver' );
  AddStr( 2003, '&Usar el programa asociado (%s)'+#13+
                '&Visualizador ASCII Interno' );
  // unit fmLastOutput
  AddStr( 2100, 'Ver último resultado' );
  // unit fmFilters
{*}  AddStr( 2200, 'Filters' );
{*}  AddStr( 2202, 'Add' );
{*}  AddStr( 2203, 'Edit' );
{*}  AddStr( 2204, 'Delete' );
{*}  AddStr( 2205, 'Clear all' );
{*}  AddStr( 2206, 'Kind of filter' );
{*}  AddStr( 2207, 'Require'+#13+
{*}                'Exclude' );
{*}  AddStr( 2208, 'Edit filter:' );
{*}  AddStr( 2209, 'New filter:' );
end;

end.

