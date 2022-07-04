{$I jedi.inc}

//Coment this line for replase SQL editor to standart strings editor
{$DEFINE FBSQLEditor}
{$DEFINE FB_USE_SQL_WAIT_CURSOR}


{$IFDEF FPC}
{.$mode objfpc}{$H+}
{$ENDIF}


{$IFDEF FPC}
  {$IFDEF  VER1_0}
     Please use FPC 1.1 or later to compile this.
  {$ELSE}
    {$DEFINE HASVARIANT}
    {$DEFINE SUPPORTS_OUTPARAMS}
    {$DEFINE SUPPORTS_WIDECHAR}
    {$DEFINE SUPPORTS_WIDESTRING}
    {$IFDEF HASINTF}
      {$DEFINE SUPPORTS_INTERFACE}
    {$ENDIF}
    {$IFDEF HASVARIANT}
      {$DEFINE SUPPORTS_VARIANT}
    {$ENDIF}
    {$IFDEF FPC_HAS_TYPE_SINGLE}
      {$DEFINE SUPPORTS_SINGLE}
    {$ENDIF}
    {$IFDEF FPC_HAS_TYPE_DOUBLE}
      {$DEFINE SUPPORTS_DOUBLE}
    {$ENDIF}
    {$IFDEF FPC_HAS_TYPE_EXTENDED}
      {$DEFINE SUPPORTS_EXTENDED}
    {$ENDIF}
    {$IFDEF HASCURRENCY}
      {$DEFINE SUPPORTS_CURRENCY}
    {$ENDIF}
    {$DEFINE SUPPORTS_THREADVAR}
    {$DEFINE SUPPORTS_CONSTPARAMS}
    {$DEFINE SUPPORTS_LONGWORD}
    {$DEFINE SUPPORTS_INT64}
    {$DEFINE SUPPORTS_DYNAMICARRAYS}
    {$DEFINE SUPPORTS_DEFAULTPARAMS}
    {$DEFINE SUPPORTS_OVERLOAD}
    {$DEFINE ACCEPT_DEPRECATED}
    {$DEFINE ACCEPT_PLATFORM}
    {$DEFINE ACCEPT_LIBRARY}
    {$DEFINE SUPPORTS_EXTSYM}
    {$DEFINE SUPPORTS_NODEFINE}

    {$DEFINE SUPPORTS_CUSTOMVARIANTS}
    {$DEFINE SUPPORTS_VARARGS}
    {$DEFINE SUPPORTS_ENUMVALUE}
    {$IFDEF LINUX}
      {$DEFINE HAS_UNIT_LIBC}
    {$ENDIF LINUX}
    {$DEFINE HAS_UNIT_TYPES}
    {$DEFINE HAS_UNIT_VARIANTS}
    {$DEFINE HAS_UNIT_STRUTILS}

    {$DEFINE XPLATFORM_RTL}

    {$UNDEF SUPPORTS_DISPINTERFACE}
    {$UNDEF SUPPORTS_IMPLEMENTS}
    {$UNDEF SUPPORTS_UNSAFE_WARNINGS}
  {$ENDIF}
  
  {nomix_class_obj marco used in delphi 2005-2005 ещ вуашту avoiding multiply 
   using objects and classes in same unit}
  {$UNDEF nomix_class_obj}
{$ELSE}
  {$IFDEF VER130}
    {$DEFINE nomix_class_obj}
  {$ENDIF}
  {$IFDEF VER140}
    {$DEFINE nomix_class_obj}
  {$ENDIF}
{$ENDIF FPC}


{$IFDEF FB_USE_SQL_WAIT_CURSOR}
{$DEFINE FB_USE_LCL}
{$ENDIF}

{WINCP1251: ето макро реально нет надобности неиспользовать. оно задействует
   механизм надежного освобождения рекордов имеющих произвольное количество ссылок.
   сейчас реально ссылки задействованы только в реализации OldValue, при етом
   неможет теоретически возникнуть список рекордов, тоесть возможно только
   одноранговое дерево в котором несколько рекордов могут ссылаться на один общий
   старый рекорд. Для того чтобы отслеживать нарушения етого правила, при освобождении
   памяти рекордов подымается исключение при попытке освободить элемент протяженного
   списка у которого есть предки.
   ето макро имеет смысл задействовать для отладки потерь памяти рекордов
 }

{$DEFINE FB_USE_FREESTYLE_REFERENCED_RECORDS}

