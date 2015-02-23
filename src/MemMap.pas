{                                                                             }
{ TMemMap v1.00                                                               }
{ Amigreen Software 1999, Janus N. Tøndering (j@nus.person.dk)                }
{                                                                             }
{ This unit is freeware. There's limited support on this unit and if it some- }
{ how does any damage to your system (which isn't likely) - DON'T BLAIM ME!   }
{ USE AT YOUR OWN RISK!                                                       }
{                                                                             }
{ Version history:                                                            }
{ - 1.00        Initial release.                                              }

unit MemMap;

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  EMemMapException = class(Exception);
  TMemMap = class(TObject)
  private
    { Private declarations }
    fMapHandle: Integer;
    fMem: Pointer;
    fDontFree: Boolean;
    function GetRealPointer: Pointer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const Name: string; const Size: Integer);
    destructor Destroy; override;
    function GetMemSize: Integer;
    procedure ReadBuffer(var Buffer);
    procedure WriteBuffer(const Buffer; const Size: Integer);
    property Memory: Pointer read fMem default nil;
  published
    { Published declarations }
    property DontFree: Boolean read fDontFree write fDontFree default True;
  end;

implementation

constructor TMemMap.Create(const Name: string; const Size: Integer);
//var lNew: Boolean;
begin
  inherited Create;
  fMem := nil; DontFree := True;

  {$IFDEF WIN64}
  fMapHandle := CreateFileMapping($FFFFFFFFFFFFFFFF, nil, PAGE_READWRITE, 0, Size + 4, PChar(Name));
  {$ELSE}
  fMapHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, Size + 4, PChar(Name));
  {$ENDIF}

  if fMapHandle = 0 then
  begin
    raise EMemMapException.Create('Error creating memory mapping');
    fMapHandle := 0;
    Destroy;
  end
  else
  begin
    //lNew := (GetLastError <> ERROR_ALREADY_EXISTS);
    fMem := MapViewOfFileEx(fMapHandle, FILE_MAP_WRITE, 0, 0, 0, nil);
    if fMem = nil then
    begin
      raise EMemMapException.Create('Error creating memory mapping');
      Destroy;
    end
    else
    begin
      PInteger(fMem)^ := Size;
      fMem := Pointer(Integer(fMem) + 4);
      //if lNew then
      //  FillChar(fMem, Size, 0);
    end;
  end;
end;

destructor TMemMap.Destroy;
begin
  if (fMem <> nil) then
    if not UnmapViewOfFile(GetRealPointer) then
      raise EMemMapException.Create('Error deallocating mapped memory');
  if (fMapHandle <> 0) and (DontFree = False) then
    if not CloseHandle(fMapHandle) then
      raise EMemMapException.Create('Error freeing memory mapping handle');

  inherited Destroy;
end;

function TMemMap.GetMemSize: Integer;
begin
   if (fMem <> nil) then
     Result := PInteger(GetRealPointer)^
   else
   begin
     //Result := 0;
     raise EMemMapException.Create('Error getting mapping size');
   end;
end;

procedure TMemMap.ReadBuffer(var Buffer);
begin
  if (fMem <> nil) then
    Move(fMem^, Buffer, GetMemSize);
end;

procedure TMemMap.WriteBuffer(const Buffer; const Size: Integer);
begin
  if (fMem <> nil) then
    if (Size <= GetMemSize) then
      Move(Buffer, fMem^, Size)
    else
      raise EMemMapException.Create('Buffer ''Size'' is too big!');
end;

function TMemMap.GetRealPointer: pointer;
begin
  if fMem <> nil then
    Result := Pointer(Integer(fMem) - 4)
  else
    Result := nil;
end;

end.
