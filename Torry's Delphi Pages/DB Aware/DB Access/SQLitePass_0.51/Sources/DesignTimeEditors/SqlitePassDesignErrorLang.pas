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
    Last update : 10.12.2009

   --------------------------------------------------------------------------- }


Unit SqlitePassDesignErrorLang;
{$i ..\..\Sources\SqlitePassDbo.inc}

Interface
//******************************************************* DatabaseError Messages
RESOURCESTRING
CRLF = #13#10;

{ TSqlitePassMasterDetailDialog }

Msg1000 = 'The MasterSource property should be defined first...';
Msg1005 = 'The MasterSource dataset is closed and the MasterSourceAutoActivate Property is set to False...';
Msg1010 = 'The Database property should be defined first...';
Msg1015 = 'Cannot display Fields information when dataset is closed.';
Msg1017 = 'Cannot display IndexDefs information when dataset is closed';
Msg1020 = 'The Database is not connected.';
Msg1025 = 'Cannot display Datatypes mapping rules.';
Msg1027 = 'Cannot display Database items.';
Msg1029 = 'The current dataset is not filtered. Do you want to activate the filter(s) now ?';
implementation
end.
