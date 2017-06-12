{                                                                             }
{ TMemMap v2.00                                                               }
{ Amigreen Software 1999, Janus N. Tndering (j@nus.person.dk)                }
{                                                                             }
{ This unit is freeware. There's limited support on this unit and if it some- }
{ how does any damage to your system (which isn't likely) - DON'T BLAIM ME!   }
{ USE AT YOUR OWN RISK!                                                       }
{                                                                             }
{ Version history:                                                            }
{ - 1.00        Initial release.                                              }
{ - 2.00        Rewrite @ 2017-04-13 (YYYY-MM-DD)                             }
{                       by Andreas Toth andreas.toth@xtra.co.nz               }

unit MemMap;

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  EMemMapException = class(Exception);

  TMemMap = class(TObject)
  private
    FName: UnicodeString;
    FSize: Cardinal;
    FFreeOnDestroy: Boolean;

    FMemory: Pointer;
    FHandle: THandle;

    procedure Clean(const APrefix: string = 'Clean');
  public
    constructor Create(const AName: UnicodeString; const ASize: Cardinal; const AFreeOnDestroy: Boolean);
    destructor Destroy; override;

    function Name: UnicodeString;
    function Size: PtrInt;
    property FreeOnDestroy: Boolean read FFreeOnDestroy write FFreeOnDestroy;

    procedure ReadBuffer(var ABuffer; const ASize: Cardinal);
    procedure WriteBuffer(const ABuffer; const ASize: Cardinal);

    property Memory: Pointer read FMemory default nil;
  end;

implementation

constructor TMemMap.Create(const AName: UnicodeString; const ASize: Cardinal; const AFreeOnDestroy: Boolean);
begin
  inherited Create;

  FName := '';
  FSize := 0;
  FFreeOnDestroy := True;
  FMemory := nil;
  FHandle := 0;

  try
    if ASize = 0 then
    begin
      raise EMemMapException.Create('Create: Invalid size');
    end;

    FHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, ASize, PChar(AName));

    if FHandle = 0 then
    begin
      raise EMemMapException.Create('Create: Error mapping memory');
    end;

    FMemory := MapViewOfFileEx(FHandle, FILE_MAP_WRITE, 0, 0, 0, nil);

    if not Assigned(FMemory) then
    begin
      raise EMemMapException.Create('Create: Error allocating memory');
    end;
  except
    try
      Clean;
    except
      // Do not raise new exception
    end;

    raise;
  end;

  FName := AName;
  FSize := ASize;
  FFreeOnDestroy := AFreeOnDestroy;
end;

destructor TMemMap.Destroy;
begin
  try
    Clean('Destroy');
  finally
    inherited Destroy;
  end;
end;

procedure TMemMap.Clean(const APrefix: string);
begin
  try
    if Assigned(FMemory) then
    begin
      if not UnmapViewOfFile(FMemory) then
      begin
        raise EMemMapException.Create(APrefix + ': Error unmapping memory');
      end;

      FMemory := nil;
    end;
  finally
    if (FHandle <> 0) and FFreeOnDestroy then
    begin
      if not CloseHandle(FHandle) then
      begin
        raise EMemMapException.Create(APrefix + ': Error deallocating memory');
      end;

      FHandle := 0;
    end;
  end;
end;

function TMemMap.Name: UnicodeString;
begin
  if not Assigned(FMemory) then
  begin
    raise EMemMapException.Create('Name: Memory not allocated');
  end;

  Result := FName;
end;

function TMemMap.Size: PtrInt;
begin
  if not Assigned(FMemory) then
  begin
    raise EMemMapException.Create('Size: Memory not allocated');
  end;

  Result := FSize;
end;

procedure TMemMap.ReadBuffer(var ABuffer; const ASize: Cardinal);
begin
  if not Assigned(FMemory) then
  begin
    raise EMemMapException.Create('ReadBuffer: Memory not allocated');
  end;

  if ASize > FSize then
  begin
    raise EMemMapException.Create('ReadBuffer: Too much data requested');
  end;

  Move(FMemory^, ABuffer, ASize);
end;

procedure TMemMap.WriteBuffer(const ABuffer; const ASize: Cardinal);
begin
  if not Assigned(FMemory) then
  begin
    raise EMemMapException.Create('WriteBuffer: Memory not allocated');
  end;

  if ASize > FSize then
  begin
    raise EMemMapException.Create('WriteBuffer: Too much data provided');
  end;

  Move(ABuffer, FMemory^, ASize);
end;

end.
