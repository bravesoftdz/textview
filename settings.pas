
unit Settings;

interface

procedure LoadSettings(
  out ADirectory: string;
  out AFilter: string;
  out AItemIndex: integer
);
procedure SaveSettings(
  const ADirectory: string;
  const AFilter: string;
  const AItemIndex: integer
);
  
implementation

uses
  SysUtils, IniFiles;
  
var
  LIniFileName: TFileName;
  
procedure LoadSettings(
  out ADirectory: string;
  out AFilter: string;
  out AItemIndex: integer
);
begin
  with TIniFile.Create(LIniFileName) do
  try
    ADirectory := ReadString('.', 'directory', '');
    AFilter := ReadString('.', 'filter', '');
    AItemIndex := ReadInteger('.', 'itemindex', -1);
  finally
    Free;
  end;
end;

procedure SaveSettings(
  const ADirectory: string;
  const AFilter: string;
  const AItemIndex: integer
);
begin
  with TIniFile.Create(LIniFileName) do
  try
    WriteString('.', 'directory', ADirectory);
    WriteString('.', 'filter', AFilter);
    WriteInteger('.', 'itemindex', AItemIndex);
    UpdateFile;
  finally
    Free;
  end;
end;

begin
  LIniFileName := ChangeFileExt(ParamStr(0), '.ini');
end.
