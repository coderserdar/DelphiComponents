{
TInactivityApp  Component Version 1.0 - Suite GLib
Copyright (©) 2007,  by Germán Estévez (Neftalí)

  [ES] Permite detectar en la aplicación donde se utiliza la inactividad de teclado y
  de ratón pasado un determinado tiempo. Configurable por el usuario.
  Basta con activar configurar los minutos y segundos de inactividad y activar
  el componente y pasado este tiempo de inactividad "saltará" un evento de aviso.

  [EN] With this component you can detect inactivity of mouse and keyboard of
  user inside your application. The time of inactivity is configurable for the
  user/programmer.
  You must set the time, in minues and seconds for the inactivity and activate
  the component. Past this time an event is activated.

=========================================================================
IMPORTANTE PROGRAMADORES: Por favor, si tienes comentarios, mejoras, ampliaciones,
  errores y/o cualquier otro tipo de sugerencia envíame un mail a:
  german_ral@hotmail.com

IMPORTANT PROGRAMMERS: please, if you have comments, improvements, enlargements,
  errors and/or any another type of suggestion send a mail to:
  german_ral@hotmail.com    
=========================================================================

@author Germán Estévez (Neftalí)
@cat Package GLib
}
unit CInactivityApp;
{
=========================================================================

  CInactivityApp.pas

  Componente

========================================================================
  Historia de las Versiones
------------------------------------------------------------------------

  10/07/2007  Germán Estévez

  * Creación.

=========================================================================

  Errores detectados no corregidos

=========================================================================
}

//=========================================================================
//
// I N T E R F A C E
//
//=========================================================================
interface

uses
  Windows, Messages, Forms, Classes, ExtCtrls;

type
  {: Clase que define el componente TInactivityApp.}
  TInactivityApp = class(TComponent)
  private
    FInactivitySeconds: integer;
    FInactivityMinutes: integer;
    FActive: Boolean;
    FInactivityIntervalCheck: Cardinal;
    FOnResetTimer: TNotifyEvent;
    FOnInactivity: TNotifyEvent;
    FOnInactivityComplete: TNotifyEvent;

    
    // Timer interno para las comprobaciones.
    _InternalTimer:TTimer;
    // Puntero al anterior evento si estaba asignado
    _OldOnMessage:TMessageEvent;
    // Momento en que comienza a contar la inactividad
    _IniInactivityTime:TDateTime;
    // Número totas de segundos (FInactivitySeconds y FInactivityMinutes)
    _TotalInactivity:Cardinal;

    _diffSec:Cardinal;

    // Ya se ha sobrepasado el tiempo...
    VHooked:boolean;



    //: Calcular tiempo de inactividad total (segundos).
    procedure _CalculateInactivity();
    //: Procedimiento de captura de mensajes de aplicación.
    procedure _NewOnMessage(var Msg : TMsg; var Handled: Boolean);
    //: procedimiento interno de comprobación de inactividad.
    procedure _InternalOnTimer(Sender: TObject);
    //: Resetear el timer interno.
    procedure _ResetTimer();

    // Procedimientos internos para proppiedades
    procedure PSetVMinutes(Value: integer);
    procedure PSetVSeconds(Value: integer);
    procedure SetActive(const Value: Boolean);


  protected

  public
    //: Redefinimos el constructor.
    constructor Create(AOwner: TComponent); override;
    //: Redefinir el destructor.
    destructor Destroy(); override;


    // DEBUG
    function __TotalInactivity:Cardinal;
    function __DifInactivity:Cardinal;


  published
    //: Intervalo de tiempo de comprobación de inactividad.
    property InactivityIntervalCheck:Cardinal read FInactivityIntervalCheck
      write FInactivityIntervalCheck;
    //: Inactividad en minutos.
    property InactivityMin:integer read FInactivityMinutes write PSetVMinutes;
    //: Inactividad en segundos.
    property InactivitySec:integer read FInactivitySeconds write PSetVSeconds;
    //: Si el componente está activo o no.
    property Active:Boolean read FActive write SetActive;

    // ======================== EVENTOS =======================================

    //: Evento cuando se cumple el tiempo de inactividad.
    property OnInactivityComplete:TNotifyEvent read FOnInactivityComplete
      write FOnInactivityComplete; 
    //: Evento que "salta" cuando se Resetea el Timer.
    property OnResetTimer:TNotifyEvent read FOnResetTimer write FOnResetTimer;
    //: Evento que salta a medida que va "contando" el Timer de Inactividad...
    property OnInactivityStep:TNotifyEvent read FOnInactivity write FOnInactivity;
    // !!!!! Cuidado con este evento ya que salta muy amenudo. Hay que tener
    // cuidado con lo que se programa dentro de él.

  end;


//: Procedimiento de registro del componente.
procedure Register();

//=========================================================================
//
// I M P L E M E N T A T I O N
//
//=========================================================================
implementation

uses
  SysUtils, DateUtils;


//: Procedimniento de registro.
procedure Register;
begin
  RegisterComponents('GLib', [TInactivityApp]);
end;

//: Sobreesccribir el constructor.
constructor TInactivityApp.Create(AOwner: TComponent);
begin

  // Metodo heredado.
  inherited;

  // Inicializaciones por defecto.
  Self.FInactivityIntervalCheck := 1000;
  Self.FInactivitySeconds := 0;
  Self.FInactivityMinutes := 10;
  Self.FActive := False;

  //+G
  Self.VHooked := False;

  // En diseño no se crea el Timer
	if (csDesigning in ComponentState) then begin
    Exit;
  end;  

  // Crear el Timer interno...
  _InternalTimer := TTimer.Create(Self);
	_InternalTimer.Interval := Self.FInactivityIntervalCheck;
	_InternalTimer.Enabled := Self.FActive and (not (csDesigning in ComponentState));
  // Capturar el evento...
	_InternalTimer.OnTimer := _InternalOnTimer;

  // Capturar los mensajes de Aplicación
  _OldOnMessage := Application.OnMessage;
  // Proc de captura
  Application.OnMessage := _NewOnMessage;

  // Resetear
  _ResetTimer();

end;



//: Redefinir el destructor de la clase.
destructor TInactivityApp.Destroy();
begin

  inherited;
end;

//: Acceso a la propiedad para escritura.
procedure TInactivityApp.PSetVMinutes(Value: integer);
begin

  // Valor aceptable?
	if (Value < 0) then begin
    //Corregir
	  Value := 0;
  end;

  // Valor aceptable?
	if (Value > 59) then begin
    //Corregir
	  Value := 59;
  end;

  // No ha cambiado?
  if (Self.FInactivityMinutes = Value) then begin
    // Salir
    Exit;
  end;

  // Asignar el nuevo
  Self.FInactivityMinutes := Value;
  // Calcular tiempo de inactividad total (segundos)
  _CalculateInactivity();

  // ResetTimer
	_ResetTimer();


end;

//: Acceso a la propiedad para escritura.
procedure TInactivityApp.PSetVSeconds(Value: integer);
begin

  // Valor aceptable?
	if (Value < 0) then begin
    // Corregir
    Value := 0;
  end;

  // Aceptable
  if (Value > 59) then begin
    Value := 59;
  end;

  // No ha cambiado?
  if (Self.FInactivitySeconds = Value) then begin
    // Salir
    Exit;
  end;

  // Asignar el nuevo
  Self.FInactivitySeconds := Value;
  // Calcular tiempo de inactividad total (segundos)
  _CalculateInactivity();
  
  // ResetTimer
	_ResetTimer();

end;


procedure TInactivityApp.SetActive(const Value: Boolean);
begin
  // No ha cambiado?
  if (Self.FActive = Value) then begin
    Exit;
  end;

  // Asignar el nuevo valor
  Self.FActive := Value;

  // no está en diseño...?  => Nada
  if not (csDesigning in ComponentState) then begin
    Self._InternalTimer.Enabled := Value;
    // Activar
    if (Active) then begin
      // Resetear
      _ResetTimer();
    end;
  end;

end;

//: Calcular tiempo de inactividad total (segundos).
procedure TInactivityApp._CalculateInactivity();
begin
  // Tiempo total en segundos;
  Self._TotalInactivity := (FInactivityMinutes * 60) + FInactivitySeconds;
end;


//: procedimiento interno de comprobación de inactividad.
procedure TInactivityApp._InternalOnTimer(Sender: TObject);
var
//  diffSec:Cardinal;
  h, s:Int64;
  n:TDateTime;
begin

  // Está en diseño...?  => Nada
  if (csDesigning in ComponentState) then begin
    Exit;
  end;

  // No activado?
  if not (Self.Active) then begin
    Exit;
  end;

  // Ya ha llegado al final?
  if (VHooked) then begin
    Exit;
  end;

  // Hora actual
  n := Now();

  // Cantidad de segundos/horas de inactividad
  h := HoursBetween(n, Self._IniInactivityTime);
  s := SecondsBetween(n, Self._IniInactivityTime);
  // Diferencia
  _diffSec := (h * 60) + s;

  // Asignado el evento de cada paso?
  if Assigned(Self.FOnInactivity) then begin
    Self.FOnInactivity(Self);
  end;

  // Se ha llegado el tiempo pactado de inactividad?
	if (_diffSec >= Self._TotalInactivity) then begin
    // Asignado el evento?
		if Assigned(Self.FOnInactivityComplete) then begin
      // Marcamos el Flag
      VHooked := True;
      // resetear el timer
      _ResetTimer();
      // levantar el evento
      Self.FOnInactivityComplete(Self);
    end;
  end;

end;


//: Procedimiento de captura de mensajes de aplicación.
procedure TInactivityApp._NewOnMessage(var Msg: TMsg; var Handled: Boolean);
begin

  // Está en diseño...?  => Nada
  if (csDesigning in ComponentState) then begin
    Exit;
  end;

  // Asiganado el evento original?
  if Assigned(Self._OldOnMessage) then begin
    // Lanzarlo...
    Self._OldOnMessage(Msg, Handled);
  end;

  // Tratar el mensaje?
	if (Msg.message = WM_MOUSEMOVE) OR (Msg.message = WM_KEYDOWN) then begin
    // Resetear el Timer
    _ResetTimer();
    // Ya se ha llegado al límite? ==> No se tratan...
    if VHooked then begin
      // Reiniciar
      VHooked := False
    end
    else begin
      // Nada
    end;
	end;
end;


//: Resetear el timer interno.
procedure TInactivityApp._ResetTimer();
//var
//	Hora,Minutos,Segundos	: integer;
//	HoraTexto24				: string;
//	HoraTexto12				: string;
begin

  // Está en diseño...?  => Nada
  if (csDesigning in ComponentState) then begin
    Exit;
  end;

  // Si no está activo, salimos
  if not (Self.FActive) then begin
    Exit;
  end;

  // Resetear el inicial
  _IniInactivityTime := Now;

  // Levantar el evento de Reset.
  if Assigned(Self.FOnResetTimer) then begin
    Self.FOnResetTimer(Self);
  end;





//	HoraTexto24 := FormatDateTime('hhmmss',Time);
//	Hora := StrToInt(Copy(HoraTexto24,1,2));
//	Minutos := StrToInt(Copy(HoraTexto24,3,2));
//	Segundos := StrToInt(Copy(HoraTexto24,5,2));
//
//	Segundos := Segundos + VSeconds;
//
//	if Segundos > 59 then
//		begin
//			inc(Minutos);
//			Segundos := Segundos - 60;
//		end;
//
//	Minutos := Minutos + VMinutes;
//
//	if Minutos > 59 then
//		begin
//			inc(Hora);
//			Minutos := Minutos - 60;
//		end;
//
//	if Hora > 23 then
//		begin
//			Hora := Hora - 24;
//		end;
//
//	HoraTexto24 := Format('%.2d:%.2d:%.2d',[Hora,Minutos,Segundos]);
//	if Hora > 12 then
//		begin
//			Hora := abs(Hora -12);
//		end;
//	HoraTexto12 := Format('%d:%d:%d',[Hora,Minutos,Segundos]);
//	try
//		VTimeOff := StrToTime(HoraTexto24);
//	except
//		VTimeOff := StrToTime(HoraTexto12);
//	end;

end;

function TInactivityApp.__DifInactivity: Cardinal;
begin
  Result := _diffSec;
end;

function TInactivityApp.__TotalInactivity: Cardinal;
begin
  Result := _TotalInactivity;
end;

end.

