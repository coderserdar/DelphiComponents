{   This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   ---------------------------------------------------------------------------

    Messages localisation unit
    Author : Luc DAVID Email: luckylazarus@free.fr
    2006-2009

    Major changes are indicated in the \Documentation\changes.pdf file
    Last update : 05.01.2009

   --------------------------------------------------------------------------- }


Unit SqlitePassVisualToolsLang;
{$i ..\..\Sources\SqlitePassDbo.inc}

Interface

ResourceString

{ TSqlitePassRes_DBActions }
SqlitePassRes_DBActionsSortAscCaption = '&Sort +';
SqlitePassRes_DBActionsSortAscHint = 'Sort the selected field on ascending order';
SqlitePassRes_DBActionsSortAscShortcut = 'CTRL+D';

SqlitePassRes_DBActionsSortDescCaption = '&Sort -';
SqlitePassRes_DBActionsSortDescHint = 'Sort the selected field on descending order';
SqlitePassRes_DBActionsSortDescShortcut = 'CTRL+U';

SqlitePassRes_DBActionsSortCaption = '&Sort';
SqlitePassRes_DBActionsSortHint = 'Sort on one or more fields';
SqlitePassRes_DBActionsSortShortcut = 'CTRL+S';

SqlitePassRes_DBActionsFilterOnSelectionCaption = '&Filter on selection';
SqlitePassRes_DBActionsFilterOnSelectionHintOn = 'Current filter is : ';
SqlitePassRes_DBActionsFilterOnSelectionHintOff = 'Filter record on the current selected field value';
SqlitePassRes_DBActionsFilterOnSelectionShortcut = '';

SqlitePassRes_DBActionsAdvancedFilterCaption = '&Filter';
SqlitePassRes_DBActionsAdvancedFilterHintOn = 'Current filter is : ';
SqlitePassRes_DBActionsAdvancedFilterHintOff = 'Advanced filter editor. No filter defined';
SqlitePassRes_DBActionsAdvancedFilterShortcut = 'CTRL+F';

SqlitePassRes_DBActionsAdvancedFilterOnOffCaption = '&Apply Filter';
SqlitePassRes_DBActionsAdvancedFilterOnOffHintOn = 'Current filter is : ';
SqlitePassRes_DBActionsAdvancedFilterOnOffHintOff = 'Filter record on defined advanced filter : ';
SqlitePassRes_DBActionsAdvancedFilterOnOffShortcut = '';

SqlitePassRes_DBActionsLocateRecordCaption = '&Locate';
SqlitePassRes_DBActionsLocateRecordHint = 'Locate one or more records';
SqlitePassRes_DBActionsLocateRecordShortcut = 'CTRL+L';


implementation
end.
