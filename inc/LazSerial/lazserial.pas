{ LazSerial v0.1
Serial Port Component for Lazarus 
by Jurassic Pork  03/2013
This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. }

{ Based on }
{ SdpoSerial v0.1.4
  CopyRight (C) 2006-2010 Paulo Costa
   paco@fe.up.pt
} 
{ Synaser library  by Lukas Gebauer }
{ TcomPort component }


{ features :
Changed :  baudrate values.
            stop bits  new value : 1.5
new event : onstatus
new property FRcvLineCRLF : if this property is true, you use RecvString
in place of RecvPacket when you read data from the port.

new procedure  ShowSetupDialog to open a port settings form :
the device combobox contain the enumerated ports.
new procedure to enumerate real serial port on linux ( in synaser).

Demo : a simulator of gps serial port + serial port receiver :
you can send NMEA frames ( GGA GLL RMC°) to the opened serial port
(start gps simulator). You can change speed and heading.
In the memo you can see what is received from  the opened serial port.
In the status bar you can see the status events.

}


unit LazSerial;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF LINUX}
  Classes,
{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}
{$ELSE}
  Windows, Classes, //registry,
{$ENDIF}
  SysUtils, synaser,  LResources, Forms, Controls, Graphics, Dialogs;


type
  TBaudRate=(br___110,br___300, br___600, br__1200, br__2400, br__4800,
             br__9600,br_14400, br_19200, br_38400,br_56000, br_57600,
             br115200,br128000, br230400,br256000, br460800, br921600);
  TDataBits=(db8bits,db7bits,db6bits,db5bits);
  TParity=(pNone,pOdd,pEven,pMark,pSpace);
  TFlowControl=(fcNone,fcXonXoff,fcHardware);
  TStopBits=(sbOne,sbOneAndHalf,sbTwo);

  TModemSignal = (msRI,msCD,msCTS,msDSR);
  TModemSignals = Set of TModemSignal;
  TStatusEvent = procedure(Sender: TObject; Reason: THookSerialReason; const Value: string) of object;

const
  ConstsBaud: array[TBaudRate] of integer=(110,
    300, 600, 1200, 2400, 4800, 9600,14400, 19200, 38400,56000, 57600,
    115200,128000,230400,256000, 460800, 921600);

  ConstsBits: array[TDataBits] of integer=(8, 7 , 6, 5);
  ConstsParity: array[TParity] of char=('N', 'O', 'E', 'M', 'S');
  ConstsStopBits: array[TStopBits] of integer=(SB1,SB1AndHalf,SB2);


type
  TLazSerial = class;

  TComPortReadThread=class(TThread)
  public
    MustDie: boolean;
    Owner: TLazSerial;
  protected
    procedure CallEvent;
    procedure Execute; override;
  published
    property Terminated;
  end;

  { TLazSerial }

  TLazSerial = class(TComponent)
  private
    FActive: boolean;
    FSynSer: TBlockSerial;
    FDevice: string;

    FBaudRate: TBaudRate;
    FDataBits: TDataBits;
    FParity: TParity;
    FStopBits: TStopBits;
    
    FSoftflow, FHardflow: boolean;
    FFlowControl: TFlowControl;
    FRcvLineCRLF : Boolean;

    FOnRxData: TNotifyEvent;
    FOnStatus: TStatusEvent;
    ReadThread: TComPortReadThread;

    procedure DeviceOpen;
    procedure DeviceClose;

    procedure ComException(str: string);

  protected
    procedure SetActive(state: boolean);
    procedure SetBaudRate(br: TBaudRate);
    procedure SetDataBits(db: TDataBits);
    procedure SetParity(pr: TParity);
    procedure SetFlowControl(fc: TFlowControl);
    procedure SetStopBits(sb: TStopBits);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    // show a port settings dialog form
    procedure ShowSetupDialog;
    // read data from port
    function DataAvailable: boolean;
    function ReadData: string;
//    function ReadBuffer(var buf; size: integer): integer;

    // write data to port
    function WriteData(data: string): integer;
    function WriteBuffer(var buf; size: integer): integer;

    // read pin states
    function ModemSignals: TModemSignals;
    function GetDSR: boolean;
    function GetCTS: boolean;
    function GetRing: boolean;
    function GetCarrier: boolean;

    // set pin states
//    procedure SetRTSDTR(RtsState, DtrState: boolean);
    procedure SetDTR(OnOff: boolean);
    procedure SetRTS(OnOff: boolean);
//  procedure SetBreak(OnOff: boolean);

  published
    property Active: boolean read FActive write SetActive;

    property BaudRate: TBaudRate read FBaudRate write SetBaudRate; // default br115200;
    property DataBits: TDataBits read FDataBits write SetDataBits;
    property Parity: TParity read FParity write SetParity;
    property FlowControl: TFlowControl read FFlowControl write SetFlowControl;
    property StopBits: TStopBits read FStopBits write SetStopBits;
    
    property SynSer: TBlockSerial read FSynSer write FSynSer;
    property Device: string read FDevice write FDevice;
    property RcvLineCRLF: Boolean read FRcvLineCRLF write FRcvLineCRLF;

    property OnRxData: TNotifyEvent read FOnRxData write FOnRxData;
    property OnStatus: TStatusEvent read FOnStatus write FOnStatus;
  end;

procedure Register;

implementation
uses LazSerialSetup;

{ TLazSerial }

procedure TLazSerial.Close;
begin
  Active:=false;
end;

procedure TLazSerial.DeviceClose;
begin
  // flush device
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Flush;
    FSynSer.Purge;
  end;
  
  // stop capture thread
  if ReadThread<>nil then begin
    ReadThread.FreeOnTerminate:=false;
    ReadThread.MustDie:= true;
    while not ReadThread.Terminated do begin
      Application.ProcessMessages;
    end;
    ReadThread.Free;
    ReadThread:=nil;
  end;

  // close device
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Flush;
    FSynSer.CloseSocket;
  end;
end;

constructor TLazSerial.Create(AOwner: TComponent);
begin
  inherited;
  //FHandle:=-1;
  ReadThread:=nil;
  FSynSer:=TBlockSerial.Create;
  FSynSer.LinuxLock:=false;
  FHardflow:=false;
  FSoftflow:=false;
  FFlowControl:=fcNone;
  {$IFDEF LINUX}
  FDevice:='/dev/ttyS0';
  {$ELSE}
  FDevice:='COM1';
  {$ENDIF}
  FRcvLineCRLF := False;;
//  FBaudRate:=br115200;
end;

function TLazSerial.DataAvailable: boolean;
begin
  if FSynSer.Handle=INVALID_HANDLE_VALUE then begin
    result:=false;
    exit;
  end;
  result:=FSynSer.CanReadEx(0);
end;

destructor TLazSerial.Destroy;
begin
  Close;
  FSynSer.Free;
  inherited;
end;

procedure TLazSerial.Open;
begin
    // Initialize OnStatus;
  if Assigned(OnStatus) then SynSer.OnStatus := OnStatus;
  Active:=true;
end;

procedure TLazSerial.ShowSetupDialog;
begin
  EditComPort(self);
end;

procedure TLazSerial.DeviceOpen;
begin
  FSynSer.Connect(FDevice);
  if FSynSer.Handle=INVALID_HANDLE_VALUE then
    raise Exception.Create('Could not open device '+ FSynSer.Device);

  FSynSer.Config(ConstsBaud[FBaudRate],
                 ConstsBits[FDataBits],
                 ConstsParity[FParity],
                 ConstsStopBits[FStopBits],
                 FSoftflow, FHardflow);

  // Launch Thread
  ReadThread := TComPortReadThread.Create(true);
  ReadThread.Owner := Self;
  ReadThread.MustDie := false;
//  ReadThread.Resume;   --> deprecated
  ReadThread.Start;
end;


function TLazSerial.ReadData: string;
begin
  result:='';
  if FSynSer.Handle=INVALID_HANDLE_VALUE then
    ComException('can not read from a closed port.');
  if FRcvLineCRLF then
  result:=FSynSer.RecvString(0)
  else
  result:=FSynSer.RecvPacket(0);
end;

procedure TLazSerial.SetActive(state: boolean);
begin
  if state=FActive then exit;

  if state then DeviceOpen
  else DeviceClose;

  FActive:=state;
end;

procedure TLazSerial.SetBaudRate(br: TBaudRate);
begin
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FBaudRate:=br;
end;

procedure TLazSerial.SetDataBits(db: TDataBits);
begin
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FDataBits:=db;
end;

procedure TLazSerial.SetFlowControl(fc: TFlowControl);
begin
  if fc=fcNone then begin
    FSoftflow:=false;
    FHardflow:=false;
  end else if fc=fcXonXoff then begin
    FSoftflow:=true;
    FHardflow:=false;
  end else if fc=fcHardware then begin
    FSoftflow:=false;
    FHardflow:=true;
  end;

  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FFlowControl:=fc;
end;

{
procedure TLazSerial.SetFlowControl(fc: TFlowControl);
begin
  if FHandle<>-1 then begin
    if fc=fcNone then CurTermIO.c_cflag:=CurTermIO.c_cflag and (not CRTSCTS)
    else CurTermIO.c_cflag:=CurTermIO.c_cflag or CRTSCTS;
    tcsetattr(FHandle,TCSADRAIN,CurTermIO);
  end;
  FFlowControl:=fc;
end;
}
procedure TLazSerial.SetParity(pr: TParity);
begin
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FParity:=pr;
end;

procedure TLazSerial.SetStopBits(sb: TStopBits);
begin
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FStopBits:=sb;
end;





function TLazSerial.WriteBuffer(var buf; size: integer): integer;
begin
//  if FSynSer.Handle=INVALID_HANDLE_VALUE then
 //   ComException('can not write to a closed port.');
  result:= FSynSer.SendBuffer(Pointer(@buf), size);
end;

function TLazSerial.WriteData(data: string): integer;
begin
  result:=length(data);
  FSynSer.SendString(data);
end;


function TLazSerial.ModemSignals: TModemSignals;
begin
  result:=[];
  if FSynSer.CTS then result := result + [ msCTS ];
  if FSynSer.carrier then result := result + [ msCD ];
  if FSynSer.ring then result := result + [ msRI ];
  if FSynSer.DSR then result := result + [ msDSR ];
end;

function TLazSerial.GetDSR: boolean;
begin
  result := FSynSer.DSR;
end;

function TLazSerial.GetCTS: boolean;
begin
  result := FSynSer.CTS;
end;

function TLazSerial.GetRing: boolean;
begin
  result := FSynSer.ring;
end;

function TLazSerial.GetCarrier: boolean;
begin
  result := FSynSer.carrier;
end;

{procedure TLazSerial.SetBreak(OnOff: boolean);
begin
//  if FHandle=-1 then
//    ComException('can not set break state on a closed port.');
//  if OnOff=false then ioctl(FHandle,TIOCCBRK,1)
//  else ioctl(FHandle,TIOCSBRK,0);
end;  }


procedure TLazSerial.SetDTR(OnOff: boolean);
begin
  FSynSer.DTR := OnOff;
end;


procedure TLazSerial.SetRTS(OnOff: boolean);
begin
  FSynSer.RTS := OnOff;
end;


procedure TLazSerial.ComException(str: string);
begin
  raise Exception.Create('ComPort error: '+str);
end;

{ TComPortReadThread }

procedure TComPortReadThread.CallEvent;
begin
  if Assigned(Owner.FOnRxData) then begin
    Owner.FOnRxData(Owner);
  end;
end;

procedure TComPortReadThread.Execute;
begin
  try
    while not MustDie do begin
      if Owner.FSynSer.CanReadEx(100) then
        Synchronize(@CallEvent);
    end;
  finally
    Terminate;
  end;

end;


procedure Register;
begin
  RegisterComponents('LazSerial', [TLazSerial]);
end;

initialization
{$i TLazSerial.lrs}

end.
