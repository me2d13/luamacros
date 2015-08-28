(******************************************************
 * lazSerialSetup                                     *
 *                                                    *
 * written by Jurassic Pork  O3/2013                  *
 * based on TComport TcomSetupFrm                     *
 *****************************************************)

unit lazserialsetup;

{$mode objfpc}{$H+}



interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LResources,lazSerial;

type
  // TLazSerial setup dialog

  { TComSetupFrm }

  TComSetupFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComComboBox1: TComboBox;
    ComComboBox2: TComboBox;
    ComComboBox3: TComboBox;
    ComComboBox4: TComboBox;
    ComComboBox5: TComboBox;
    ComComboBox6: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure EditComPort(ComPort: TlazSerial);

// conversion functions
function StrToBaudRate(Str: string): TBaudRate;
function StrToStopBits(Str: string): TStopBits;
function StrToDataBits(Str: string): TDataBits;
function StrToParity(Str: string): TParity;
function StrToFlowControl(Str: string): TFlowControl;
function BaudRateToStr(BaudRate: TBaudRate): string;
function StopBitsToStr(StopBits: TStopBits): string;
function DataBitsToStr(DataBits: TDataBits): string;
function ParityToStr(Parity: TParity): string;
function FlowControlToStr(FlowControl: TFlowControl): string;

implementation

uses synaser;

const
  BaudRateStrings: array[TBaudRate] of string = ('110', '300', '600',
    '1200', '2400', '4800', '9600', '14400', '19200', '38400', '56000', '57600',
    '115200', '128000', '230400', '256000','460800', '921600');
  StopBitsStrings: array[TStopBits] of string = ('1', '1.5', '2');
  DataBitsStrings: array[TDataBits] of string = ('8', '7', '6', '5');
  ParityBitsStrings: array[TParity] of string = ('None', 'Odd', 'Even',
    'Mark', 'Space');
  FlowControlStrings: array[TFlowControl] of string = ('None',
    'Software', 'HardWare');

procedure StringArrayToList(AList: TStrings; const AStrings: array of string);
var
 Cpt: Integer;
begin
  for Cpt := Low(AStrings) to High(AStrings) do
   AList.Add(AStrings[Cpt]);
end;



// string to baud rate
function StrToBaudRate(Str: string): TBaudRate;
var
  I: TBaudRate;
begin
  I := Low(TBaudRate);
  while (I <= High(TBaudRate)) do
  begin
    if UpperCase(Str) = UpperCase(BaudRateToStr(TBaudRate(I))) then
      Break;
    I := Succ(I);
  end;
  if I > High(TBaudRate) then
    Result := br__9600
  else
    Result := I;
end;

// string to stop bits
function StrToStopBits(Str: string): TStopBits;
var
  I: TStopBits;
begin
  I := Low(TStopBits);
  while (I <= High(TStopBits)) do
  begin
    if UpperCase(Str) = UpperCase(StopBitsToStr(TStopBits(I))) then
      Break;
    I := Succ(I);
  end;
  if I > High(TStopBits) then
    Result := sbOne
  else
    Result := I;
end;

// string to data bits
function StrToDataBits(Str: string): TDataBits;
var
  I: TDataBits;
begin
  I := Low(TDataBits);
  while (I <= High(TDataBits)) do
  begin
    if UpperCase(Str) = UpperCase(DataBitsToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TDataBits) then
    Result := db8bits
  else
    Result := I;
end;

// string to parity
function StrToParity(Str: string): TParity;
var
  I: TParity;
begin
  I := Low(TParity);
  while (I <= High(TParity)) do
  begin
    if UpperCase(Str) = UpperCase(ParityToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TParity) then
    Result := pNone
  else
    Result := I;
end;

// string to flow control
function StrToFlowControl(Str: string): TFlowControl;
var
  I: TFlowControl;
begin
  I := Low(TFlowControl);
  while (I <= High(TFlowControl)) do
  begin
    if UpperCase(Str) = UpperCase(FlowControlToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TFlowControl) then
    Result := fcNone
  else
    Result := I;
end;

// baud rate to string
function BaudRateToStr(BaudRate: TBaudRate): string;
begin
  Result := BaudRateStrings[BaudRate];
end;

// stop bits to string
function StopBitsToStr(StopBits: TStopBits): string;
begin
  Result := StopBitsStrings[StopBits];
end;

// data bits to string
function DataBitsToStr(DataBits: TDataBits): string;
begin
  Result := DataBitsStrings[DataBits];
end;

// parity to string
function ParityToStr(Parity: TParity): string;
begin
  Result := ParityBitsStrings[Parity];
end;

// flow control to string
function FlowControlToStr(FlowControl: TFlowControl): string;
begin
  Result := FlowControlStrings[FlowControl];
end;

procedure EditComPort(ComPort: TLazSerial);
begin
  with TComSetupFrm.Create(nil) do
  begin
    ComComboBox1.Text := ComPort.Device;
    ComComboBox2.Text :=  BaudRateToStr(ComPort.BaudRate);
    ComComboBox3.Text :=  DataBitsToStr(ComPort.DataBits);
    ComComBoBox4.Text :=  StopBitsToStr(ComPort.StopBits);
    ComComBoBox5.Text :=  ParityToStr(ComPort.Parity);
    ComComBoBox6.Text :=  FlowControlToStr(ComPort.FlowControl);

 if ShowModal = mrOK then
    begin
      ComPort.Close;
      ComPort.Device := ComComboBox1.Text;
      ComPort.BaudRate := StrToBaudRate(ComComboBox2.Text);
      ComPort.DataBits := StrToDataBits(ComComboBox3.Text);
      ComPort.StopBits := StrToStopBits(ComComboBox4.Text);
      ComPort.Parity := StrToParity(ComComboBox5.Text);
      ComPort.FlowCOntrol := StrToFlowControl(ComComboBox6.Text);
      // ComPort.Open;
    end;
    Free;
  end;
end;

{ TComSetupFrm }


procedure TComSetupFrm.FormCreate(Sender: TObject);
begin
  ComComboBox1.Items.CommaText :=  GetSerialPortNames();
  StringArrayToList(ComComboBox2.Items,BaudRateStrings) ;
  StringArrayToList(ComComboBox3.Items,DataBitsStrings) ;
  StringArrayToList(ComComboBox4.Items,StopBitsStrings) ;
  StringArrayToList(ComComboBox5.Items,ParityBitsStrings) ;
  StringArrayToList(ComComboBox6.Items,FlowControlStrings) ;
end;


initialization
  {$i lazSerialSetup.lrs}
end.
