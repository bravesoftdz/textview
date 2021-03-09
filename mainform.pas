unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ShellCtrls, FileCtrl, StdCtrls, EditBtn, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    DirectoryEdit1: TDirectoryEdit;
    FileListBox1: TFileListBox;
    FilterComboBox1: TFilterComboBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ShellTreeView1: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure FileListBox1Click(Sender: TObject);
    procedure FilterComboBox1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItem2Click(Sender: TObject);
    procedure ShellTreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    procedure ChangeDirectory(const ADirectory: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Settings
{$ifdef mswindows}
  , ShlObj
{$endif}
  ;

{$ifdef mswindows}
function GetSpecialFolder(const CSIDL: Integer): WideString;
var
  SpecialPath: PWideChar;
begin
  Result := '';
  SpecialPath := WideStrAlloc(MAX_PATH);
  try
    FillChar(SpecialPath^, MAX_PATH, 0);
    if SHGetSpecialFolderPathW(0, SpecialPath, CSIDL, False) then
      Result := SpecialPath;
  finally
    StrDispose(SpecialPath);
  end;
end;
{$endif}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
var
  LDirectory, LFilter: string;
  LItemIndex: integer;
begin
  Caption := 'TextView ' + {$I version.inc};
  Splitter1.Height := Self.ClientHeight - 2 * 8;
  LoadSettings(LDirectory, LFilter, LItemIndex);
  if DirectoryExists(LDirectory) then
    DirectoryEdit1.Directory := LDirectory
  else
    DirectoryEdit1.Directory :=
{$ifdef mswindows}
      string(GetSpecialFolder(CSIDL_PERSONAL))
{$else}
      GetEnvironmentVariable('HOME')
{$endif}
    ;
  if Length(LFilter) > 0 then
    FilterComboBox1.Filter := LFilter;
  if LItemIndex >= 0 then
  begin
    FilterComboBox1.ItemIndex := LItemIndex;
    FilterComboBox1Change(Sender);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings(DirectoryEdit1.Directory, FilterComboBox1.Filter, FilterComboBox1.ItemIndex);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ShellTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  ChangeDirectory(ExcludeTrailingPathDelimiter(ShellTreeView1.Path));
end;

procedure TForm1.ChangeDirectory(const ADirectory: string);
begin
  if DirectoryEdit1.Directory <> ADirectory then DirectoryEdit1.Directory := ADirectory;
  if ExcludeTrailingPathDelimiter(ShellTreeView1.Path) <> ADirectory then ShellTreeView1.Path := IncludeTrailingPathDelimiter(ADirectory);
  if FileListBox1.Directory <> ADirectory then FileListBox1.Directory := ADirectory;
end;

procedure TForm1.FilterComboBox1Change(Sender: TObject);
begin
  FileListBox1.Mask := FilterComboBox1.Mask;
  FileListBox1.Refresh;
end;

procedure TForm1.FileListBox1Click(Sender: TObject);
begin
  Memo1.Lines.LoadFromFile(FileListBox1.FileName);
  Caption := FileListBox1.FileName;
end;

procedure TForm1.DirectoryEdit1Change(Sender: TObject);
begin
  Hint := DirectoryEdit1.Directory;
  ChangeDirectory(DirectoryEdit1.Directory);
end;

end.

