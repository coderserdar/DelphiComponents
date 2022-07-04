unit ICQLang;
{************************************************
    For updates checkout: http://www.cobans.net
      (C) Alex Demchenko(alex@ritlabs.com)
          Gene Reeves(notgiven2k@lycos.com)
*************************************************}
interface
type
  TICQLangType = (
    LANG_EN {English}, 
    LANG_RU {Russian},
    LANG_HU {Hungarian},
    LANG_NO {Norwegian},
    LANG_DE {German}
    {Add new language identifiers here}
  );
  TTranslateFunction = function(ResID: LongWord): String;
  TICQLangNode = record
    Lang: TICQLangType;
    Translate: TTranslateFunction;
  end;
  function LangEn(ResID: LongWord): String;
  function LangRu(ResID: LongWord): String;
  function LangHu(ResID: LongWord): String;
  function LangNo(ResID: LongWord): String;
  function LangDe(ResID: LongWord): String;
  {Add new translate-functions here}
const
  ICQLanguages: array[Low(TICQLangType)..High(TICQLangType)] of TICQLangNode = (
    (Lang: LANG_EN; Translate: LangEn),
    (Lang: LANG_RU; Translate: LangRu),
    (Lang: LANG_HU; Translate: LangHu),
    (Lang: LANG_NO; Translate: LangNo),
    (Lang: LANG_DE; Translate: LangDe)
    {Add new lines here}
  );
  {Resource string identifiers, do not modify}
  IMSG_BASE                     = 0;
  IMSG_EPROTO_LEN               = IMSG_BASE + 1;
  IMSG_EHTTP_INIT               = IMSG_BASE + 2;
  IMSG_EMALFORMED_PKT           = IMSG_BASE + 3;
  IMSG_WADD_USER                = IMSG_BASE + 4;
  IMSG_EMALFORMED_LOGIN_PKT     = IMSG_BASE + 5;
  IMSG_EBAD_PASS                = IMSG_BASE + 6;
  IMSG_EOFTEN_LOGINS            = IMSG_BASE + 7;
  IMSG_WDC_BAD_PROXY            = IMSG_BASE + 8;
  IMSG_ECON_TIMEDOUT            = IMSG_BASE + 9;
  IMSG_ESOCK_SEND               = IMSG_BASE + 10;
  IMSG_ESOCK_RESOLVE            = IMSG_BASE + 11;
  IMSG_ESOCK_CONNECT            = IMSG_BASE + 12;
  IMSG_ESOCK_RECV               = IMSG_BASE + 13;
  IMSG_ESOCK_SOCKET             = IMSG_BASE + 14;
  IMSG_ESOCK_SOCKS4CONN         = IMSG_BASE + 15;
  IMSG_ESOCK_SOCKS5AUTH         = IMSG_BASE + 16;
  IMSG_ESOCK_SOCKS5NA           = IMSG_BASE + 17;
  IMSG_ESOCK_SOCKS5CONN         = IMSG_BASE + 18;
  IMSG_ESOCK_HTTPSTAT           = IMSG_BASE + 19;
  IMSG_ESOCK_HTTPBUF            = IMSG_BASE + 20;
  IMSG_ESOCK_ACCEPT             = IMSG_BASE + 21;
  IMSG_ESOCK_BIND               = IMSG_BASE + 22;
  IMSG_ESOCK_LISTEN             = IMSG_BASE + 23;
  IMSG_EDB_EFILEOPEN            = IMSG_BASE + 24;
  IMSG_EDB_EDBVERNOTSUPPORTED   = IMSG_BASE + 25;
implementation
  function LangEn(ResID: LongWord): String;
  begin
    case ResID of
      {ICQClient}
      IMSG_EPROTO_LEN:                  Result := 'Length of incoming packet exceeds maximum supported by protocol';
      IMSG_EHTTP_INIT:                  Result := 'Could not initialize connection through HTTP proxy, please retry';
      IMSG_EMALFORMED_PKT:              Result := 'Received malformed packet';
      IMSG_WADD_USER:                   Result := 'Could not add user for sending/receiving files';
      IMSG_EMALFORMED_LOGIN_PKT:        Result := 'Received malformed login packet';
      IMSG_EBAD_PASS:                   Result := 'Bad password';
      IMSG_EOFTEN_LOGINS:               Result := 'Too often logins';
      IMSG_WDC_BAD_PROXY:               Result := 'Cannot establish direct connection because remote client uses unknown proxy type';
      IMSG_ECON_TIMEDOUT:               Result := 'Connection timed out';
      {ICQSock}
      IMSG_ESOCK_SEND:                  Result := 'Could not send data';
      IMSG_ESOCK_RESOLVE:               Result := 'Could not resolve host';
      IMSG_ESOCK_CONNECT:               Result := 'Could not connect';
      IMSG_ESOCK_RECV:                  Result := 'Could not receive data';
      IMSG_ESOCK_SOCKET:                Result := 'Could not create stream socket';
      IMSG_ESOCK_SOCKS4CONN:            Result := 'SOCKS4 server cannot connect to remote server';
      IMSG_ESOCK_SOCKS5AUTH:            Result := 'Auth methods are not supported by SOCKS5 server';
      IMSG_ESOCK_SOCKS5NA:              Result := 'SOCKS5 server cannot authenticate';
      IMSG_ESOCK_SOCKS5CONN:            Result := 'SOCKS5 server cannot connect to remote server';
      IMSG_ESOCK_HTTPSTAT:              Result := 'Http proxy returned invalid status: ';
      IMSG_ESOCK_HTTPBUF:               Result := 'Http proxy buffer overflow';
      IMSG_ESOCK_ACCEPT:                Result := 'Could not accept incoming client';
      IMSG_ESOCK_BIND:                  Result := 'Could not bind server';
      IMSG_ESOCK_LISTEN:                Result := 'Could not listen incoming connections';
      {ICQDb}
      IMSG_EDB_EFILEOPEN:               Result := 'Could not open database files';
      IMSG_EDB_EDBVERNOTSUPPORTED:      Result := 'Dat version not supported';
    else
      Result := '';
    end;
  end;
  function LangRu(ResID: LongWord): String;
  begin
    case ResID of
      {ICQClient}
      IMSG_EPROTO_LEN:                  Result := 'Размер входящего пакета превышает максимально допустимое значение';
      IMSG_EHTTP_INIT:                  Result := 'Невозможно инициализировать подключение через HTTP прокси, пожалуйста повторите';
      IMSG_EMALFORMED_PKT:              Result := 'Получен неверно сформированный сетевой пакет';
      IMSG_WADD_USER:                   Result := 'Невозможно добавить пользователя для посылки/приема файлов';
      IMSG_EMALFORMED_LOGIN_PKT:        Result := 'Получен неверно сформированный login пакет';
      IMSG_EBAD_PASS:                   Result := 'Неверный пароль';
      IMSG_EOFTEN_LOGINS:               Result := 'Вход в сеть ICQ происходит слишком часто, подождите 10 минут';
      IMSG_WDC_BAD_PROXY:               Result := 'Невозможно установить прямое подключение, т.к. тип прокси пользователя неизвестен';
      IMSG_ECON_TIMEDOUT:               Result := 'Время подключения истекло';
      {ICQSock}
      IMSG_ESOCK_SEND:                  Result := 'Невозможно отослать информацию';
      IMSG_ESOCK_RESOLVE:               Result := 'Невозможно получить IP хоста';
      IMSG_ESOCK_CONNECT:               Result := 'Невозможно подключиться';
      IMSG_ESOCK_RECV:                  Result := 'Невозможно получить информацию';
      IMSG_ESOCK_SOCKET:                Result := 'Невозможно создать сокет';
      IMSG_ESOCK_SOCKS4CONN:            Result := 'SOCKS4 прокси не может соединиться с удаленным сервером';
      IMSG_ESOCK_SOCKS5AUTH:            Result := 'На SOCKS5 прокси сервере не найдены подходящие методы аторизации';
      IMSG_ESOCK_SOCKS5NA:              Result := 'SOCKS5 прокси не может авторизовать пользователя';
      IMSG_ESOCK_SOCKS5CONN:            Result := 'SOCKS5 прокси не может соединиться с удаленным сервером';
      IMSG_ESOCK_HTTPSTAT:              Result := 'HTTP прокси вернула неверный статус: ';
      IMSG_ESOCK_HTTPBUF:               Result := 'Переполнение буффера HTTP';
      IMSG_ESOCK_ACCEPT:                Result := 'Невозможно выполнить функцию Accept';
      IMSG_ESOCK_BIND:                  Result := 'Невозможно запустить сервер';
      IMSG_ESOCK_LISTEN:                Result := 'Невозможно установить сокет в статус ожидания соединений';
      {ICQDb}
      IMSG_EDB_EFILEOPEN:               Result := 'Невозможно открыть базу данных';
      IMSG_EDB_EDBVERNOTSUPPORTED:      Result := 'Версия базы данных не поддерживается';
    else
      {If we cannot translate, return the english version of the string}
      Result := ICQLanguages[LANG_EN].Translate(ResID);
    end;
  end;
  function LangHu(ResID: LongWord): String; // Translated by Mickey (mickey777@mail.datanet.hu)
  begin
    case ResID of
      {ICQClient}
      IMSG_EPROTO_LEN:                  Result := 'A bejцvх csomag hossza tъllйpi a protokoll бltal megengedettet';
      IMSG_EHTTP_INIT:                  Result := 'Nem tudok csatlakozni HTTP proxy-n keresztьl. Kйrem, prуbбlkozzon ъjra';
      IMSG_EMALFORMED_PKT:              Result := 'Deformбlt csomag йrkezett';
      IMSG_WADD_USER:                   Result := 'Nem tudok felhasznбlуt hozzбadni fбjlok kьldйsйhez/fogadбsбhoz';
      IMSG_EMALFORMED_LOGIN_PKT:        Result := 'Deformбlt bejelentkezйsi csomag йrkezett';
      IMSG_EBAD_PASS:                   Result := 'Hibбs jelszу';
      IMSG_EOFTEN_LOGINS:               Result := 'Tъl gyakori bejelentkezйsek';
      IMSG_WDC_BAD_PROXY:               Result := 'Nem tudok kцzvetlen kapcsolatot lйtesнteni, mert a tбvoli felhasznбlу ismeretlen proxyt hasznбl';
      IMSG_ECON_TIMEDOUT:               Result := 'Idхtъllйpйsi hiba, kapcsolуdбs kцzben';
      {ICQSock}
      IMSG_ESOCK_SEND:                  Result := 'Nem tudok adatokat kьldeni';
      IMSG_ESOCK_RESOLVE:               Result := 'Nem talбlom a kiszolgбlуt';
      IMSG_ESOCK_CONNECT:               Result := 'Csatlakozбsi hiba';
      IMSG_ESOCK_RECV:                  Result := 'Nincs vett adat';
      IMSG_ESOCK_SOCKET:                Result := 'A Socket lйtrehozбsa sikertelen';
      IMSG_ESOCK_SOCKS4CONN:            Result := 'A SOCKS4 kiszolgбlу nem tud csatlakozni a tбvoli kiszolgбlуhoz';
      IMSG_ESOCK_SOCKS5AUTH:            Result := 'A hitelesнtйsi eljбrбst nem tбmogatja a SOCKS5 kiszolgбlу';
      IMSG_ESOCK_SOCKS5NA:              Result := 'A SOCKS5 kiszolgбlу nem tud hitelesнteni';
      IMSG_ESOCK_SOCKS5CONN:            Result := 'A SOCKS5 kiszolgбlу nem tud csatlakozni a tбvoli kiszolgбlуhoz';
      IMSG_ESOCK_HTTPSTAT:              Result := 'A Http proxy a kцvetkezх йrvйnytelen бllapotjelzйst adta vissza: ';
      IMSG_ESOCK_HTTPBUF:               Result := 'A Http proxy puffere tъlcsordult';
      IMSG_ESOCK_ACCEPT:                Result := 'Nem tudom elfogadni a beйrkezх klienst';
      IMSG_ESOCK_BIND:                  Result := 'Nem tudok csatlakozni a szerverhez';
      IMSG_ESOCK_LISTEN:                Result := 'Nem йszlelek beйrkezх csatlakozбsokat';
      {ICQDb}
      IMSG_EDB_EFILEOPEN:               Result := 'Az adatbбzis fбjlokat nem lehet megnyitni';
      IMSG_EDB_EDBVERNOTSUPPORTED:      Result := 'Йrvйnytelen Dat verziу';
    else
      {If we cannot translate, return the english version of the string}
      Result := ICQLanguages[LANG_EN].Translate(ResID);
    end;
  end;
  function LangNo(ResID: LongWord): String; // Translated by Dan Michael (danm@start.no)
  begin
    case ResID of
      {ICQClient}
      IMSG_EPROTO_LEN:                  Result := 'Stшrrelsen pе en innkommende datapakke overskrider maksimum stшrrelse stшttet av protokollen';
      IMSG_EHTTP_INIT:                  Result := 'Kunne ikke initialisere en tilkobling via HTTP proxy, vennligst prшv igjen';
      IMSG_EMALFORMED_PKT:              Result := 'Mottok en шdelagt datapakke';
      IMSG_WADD_USER:                   Result := 'Kunne ikke invitere brukeren til е sende/motta filer';
      IMSG_EMALFORMED_LOGIN_PKT:        Result := 'Mottok en шdelagt innloggingspakke';
      IMSG_EBAD_PASS:                   Result := 'Feil passord';
      IMSG_EOFTEN_LOGINS:               Result := 'Du har logget inn for ofte';
      IMSG_WDC_BAD_PROXY:               Result := 'Kunne ikke opprette direkte tilkobling, fordi mottakeren bruker en ukjent proxy type';
      IMSG_ECON_TIMEDOUT:               Result := 'Det oppstod et tidsavbrudd';
      {ICQSock}
      IMSG_ESOCK_SEND:                  Result := 'Kunne ikke sende data';
      IMSG_ESOCK_RESOLVE:               Result := 'Kunne ikke finne serveren';
      IMSG_ESOCK_CONNECT:               Result := 'Kunne ikke koble til serveren';
      IMSG_ESOCK_RECV:                  Result := 'Kunne ikke motta data';
      IMSG_ESOCK_SOCKET:                Result := 'Kunne ikke opprette en stream socket';
      IMSG_ESOCK_SOCKS4CONN:            Result := 'SOCKS4 proxyen kunne ikke koble til serveren';
      IMSG_ESOCK_SOCKS5AUTH:            Result := 'Autentiseringsmetodene er ikke stшttet av SOCKS5 serveren';
      IMSG_ESOCK_SOCKS5NA:              Result := 'SOCKS5 proxyen kunne ikke autentisere deg';
      IMSG_ESOCK_SOCKS5CONN:            Result := 'SOCKS5 proxyen kunne ikke koble til serveren';
      IMSG_ESOCK_HTTPSTAT:              Result := 'Http proxy returnerte ugyldig statuskode: ';
      IMSG_ESOCK_HTTPBUF:               Result := 'Http proxy buffer overflow';
      IMSG_ESOCK_ACCEPT:                Result := 'Kunne ikke akseptere innkomende klient';
      IMSG_ESOCK_BIND:                  Result := 'Kan ikke binde server';
      IMSG_ESOCK_LISTEN:                Result := 'Kan ikke vente pе innkommende tilkoblinger';
      {ICQDb}
      IMSG_EDB_EFILEOPEN:               Result := 'Kan ikke еpne databasefiler';
      IMSG_EDB_EDBVERNOTSUPPORTED:      Result := 'Dat versjonen er ikke stшttet';
    else
      Result := ICQLanguages[LANG_EN].Translate(ResID);
    end;
  end;

  function LangDe(ResID: LongWord): String;
  begin
    case ResID of
      {ICQClient}
      IMSG_EPROTO_LEN:                  Result := 'Die Lдnge der eingehenden Packete ьberschreitet das maximum des Protokolls';
      IMSG_EHTTP_INIT:                  Result := 'Verbindung konnte nicht ьber den Proxy hergestellt werden, bitte nochmal versuchen';
      IMSG_EMALFORMED_PKT:              Result := 'Defekte Packete empfangen';
      IMSG_WADD_USER:                   Result := 'User konnte nicht zum senden/empfangen von Datein hinzugefьgt werden';
      IMSG_EMALFORMED_LOGIN_PKT:        Result := 'Defektes Login-Packet empfangen';
      IMSG_EBAD_PASS:                   Result := 'Falsches Passwort';
      IMSG_EOFTEN_LOGINS:               Result := 'Zu oft eingeloggt';
      IMSG_WDC_BAD_PROXY:               Result := 'Direktverbinndung konnte nicht hergestellt werden, Remote Klient verwendet einen unbekannten Proxy';
      IMSG_ECON_TIMEDOUT:               Result := 'Verbindungs Time-Out';
      {ICQSock}
      IMSG_ESOCK_SEND:                  Result := 'Daten konnten nicht gesendet werden';
      IMSG_ESOCK_RESOLVE:               Result := 'Host konnte nicht aufgelцst werden';
      IMSG_ESOCK_CONNECT:               Result := 'Verbindung konnte nicht hergestellt werden';
      IMSG_ESOCK_RECV:                  Result := 'Daten konnten nicht empfangen werden';
      IMSG_ESOCK_SOCKET:                Result := 'Stream Socket konnte nicht erstellt werden';
      IMSG_ESOCK_SOCKS4CONN:            Result := 'SOCKS4 Server konnte sich nicht zum Remote Rerver verbinden';
      IMSG_ESOCK_SOCKS5AUTH:            Result := 'Anmeldemethode wird nicht vom SOCKS5 Server unterstьzt';
      IMSG_ESOCK_SOCKS5NA:              Result := 'Konnte nicht am SOCKS5 Server anmelden';
      IMSG_ESOCK_SOCKS5CONN:            Result := 'SOCKS5 Server konnte keine Verbindung zum Remote Server herstellen';
      IMSG_ESOCK_HTTPSTAT:              Result := 'Http Proxy gab einen fehlerhaften Status zurьck: ';
      IMSG_ESOCK_HTTPBUF:               Result := 'Http Proxy Puffer ьberlauf';
      IMSG_ESOCK_ACCEPT:                Result := 'Eingehender Client konnte nicht akzeptiert werden';
      IMSG_ESOCK_BIND:                  Result := 'Konnte nicht zum Server verbinden';
      IMSG_ESOCK_LISTEN:                Result := 'Konnte keine eingehenden Verbinungen festellen';
      {ICQDb}
      IMSG_EDB_EFILEOPEN:               Result := 'Konnte Datenbank nicht цffnen';
      IMSG_EDB_EDBVERNOTSUPPORTED:      Result := 'Datenbankversion nicht unterstьzt';
    else
      Result := ICQLanguages[LANG_EN].Translate(ResID);
    end;
  end;
end.
