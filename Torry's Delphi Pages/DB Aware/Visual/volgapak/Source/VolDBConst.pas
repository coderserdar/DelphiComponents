//---------------------------------------------------------------------------
//  Resource strings for TVolgaCalendar, TVolgaDBGrid, TVolgaDBEdit
//---------------------------------------------------------------------------
//  Copyright © 2000, Olga Vlasova, Russia
//  http://volgatable.chat.ru
//  E-mail: volgatable@chat.ru
//---------------------------------------------------------------------------
unit VolDBConst;
{$DEFINE LAN_ENGLISH}
//{$DEFINE LAN_GERMAN}
//{$DEFINE LAN_RUSSIAN}

interface

{$IFDEF LAN_ENGLISH} 
resourcestring
   V_PREVYEAR = 'Previous year';
   V_PREVMON = 'Previous month';
   V_NEXTYEAR = 'Next year';
   V_NEXTMON = 'Next month';
   V_PUSHMON = 'Click to change month';
   V_TODAY = 'Today is ';
   V_PUSHTODAY = 'Click to select today';
   V_DELETEROWS = 'Delete selected records ?';
   V_DELETEONEROW = 'Delete record ?';
   V_LOOKUPSOURCEERROR = 'Lookup Dataset cannot be equal to Source DataSet';
   V_SHORTYEAR = ' y.';
   V_LONGYEAR = ' year';
   V_KINDMON = 'Month';
   V_KINDQUART = 'Quarter';
   V_KINDHALF = 'Half Year';
   V_KINDYEAR = 'Whole year';
   V_FRMTQUART = '%d Quarter %d year';
   V_FRMTHALFYEAR = '%d Half Year %d year';
   V_OKBTNHINT = 'Save result';
   V_CANCELBTNHINT = 'Close without saving';
   V_PANELFRACT = 'Decimal digits';
   V_BTNUPHINT = 'Increase number of decimal digits';
   V_BTNDOWNHINT = 'Decrease number of decimal digits';
{$ENDIF}

{$IFDEF LAN_GERMAN}
resourcestring
   V_PREVYEAR = 'vorheriges Jahr';
   V_PREVMON = 'vorheriger Monat';
   V_NEXTYEAR = 'nдchstes Jahr';
   V_NEXTMON = 'nдchster Monat';
   V_PUSHMON = 'Klicken, um den Monat zu дndern';
   V_TODAY = 'Heute ist ';
   V_PUSHTODAY = 'Klicken Sie hier, um den heutigen Tag auszuwдhlen';
   V_DELETEROWS = 'Wollen Sie die ausgewдhlten Datensдtze wirklichlцschen?';
   V_DELETEONEROW = 'Datensatz lцschen?';
   V_LOOKUPSOURCEERROR = 'Lookup Dataset kann nichtmit dem Source DataSet identisch sein';
   V_SHORTYEAR = '';
   V_LONGYEAR = ' (ges.Jahr)';
   V_KINDMON = 'Monat';
   V_KINDQUART = 'Quartal ';
   V_FRMTQUART = '%d. Quartal %d';
   V_KINDHALF = 'Halbjahr ';
   V_FRMTHALFYEAR = '%d. Halbjahr %d';
   V_KINDYEAR = 'gesamtes Jahr';
   V_OKBTNHINT = 'Speichern';
   V_CANCELBTNHINT = 'Ohne speichern schliessen';
   V_PANELFRACT = 'Dezimalstellen';
   V_BTNUPHINT = 'Dezimalstelle erhцhen';
   V_BTNDOWNHINT = 'Dezimalstelle verringern';
{$ENDIF}

{$IFDEF LAN_RUSSIAN}
resourcestring
   V_PREVYEAR = 'Предыдущий год';
   V_PREVMON = 'Предыдущий месяц';
   V_NEXTYEAR = 'Следующий год';
   V_NEXTMON = 'Следующий месяц';
   V_PUSHMON = 'Нажмите для изменения месяца';
   V_TODAY = 'Сегодня ';
   V_PUSHTODAY = 'Нажмите для выбора текущей даты';
   V_DELETEROWS = 'Удалить выбранные записи ?';
   V_DELETEONEROW = 'Удалить запись ?';
   V_LOOKUPSOURCEERROR = 'Lookup Dataset cannot be equal to Source DataSet';
   V_SHORTYEAR = ' г.';
   V_LONGYEAR = ' год';
   V_KINDMON = 'Месяц';
   V_KINDQUART = 'Квартал';
   V_KINDHALF = 'Полугодие';
   V_KINDYEAR = 'Год';
   V_FRMTQUART = '%d квартал %d года';
   V_FRMTHALFYEAR = '%d полугодие %d года';
   V_OKBTNHINT = 'Сохранить';
   V_CANCELBTNHINT = 'Закрыть без сохранения';
   V_PANELFRACT = 'Десятичные знаки';
   V_BTNUPHINT = 'Увеличить количество десятичных знаков';
   V_BTNDOWNHINT = 'Уменьшить количество десятичных знаков';
{$ENDIF}

implementation

end.
