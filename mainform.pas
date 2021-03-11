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
    procedure LoadFirstItem;
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
  , LazLogger;

{$ifdef mswindows}
function GetSpecialFolder(const CSIDL: Integer): WideString;
var
  LPath: PWideChar;
begin
  result := '';
  LPath := WideStrAlloc(MAX_PATH);
  try
    FillChar(LPath^, MAX_PATH, 0);
    if SHGetSpecialFolderPathW(0, LPath, CSIDL, FALSE) then
      result := LPath;
  finally
    StrDispose(LPath);
  end;
end;
{$endif}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
var
  LDirectory, LFilter: string;
  LItemIndex: integer;
begin
  DebugLn('FormActivate');
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
  DebugLn('FormClose');
  SaveSettings(DirectoryEdit1.Directory, FilterComboBox1.Filter, FilterComboBox1.ItemIndex);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ShellTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  DebugLn('ShellTreeView1Change');
  ChangeDirectory(ExcludeTrailingPathDelimiter(ShellTreeView1.Path));
end;

procedure TForm1.ChangeDirectory(const ADirectory: string);
begin
  DebugLn(Format('ChangeDirectory(%s)', [ADirectory]));
  if DirectoryEdit1.Directory <> ADirectory then DirectoryEdit1.Directory := ADirectory;
  if ExcludeTrailingPathDelimiter(ShellTreeView1.Path) <> ADirectory then ShellTreeView1.Path := IncludeTrailingPathDelimiter(ADirectory);
  if FileListBox1.Directory <> ADirectory then
  begin
    FileListBox1.Directory := ADirectory;
    LoadFirstItem;
  end;
end;

procedure TForm1.LoadFirstItem;
begin
  DebugLn('LoadFirstItem');
  if FileListBox1.Items.Count > 0 then
  begin
    FileListBox1.ItemIndex := 0;
    FileListBox1.Click;
  end else
    Memo1.Clear;
end;

procedure TForm1.FilterComboBox1Change(Sender: TObject);
begin
  DebugLn('FilterComboBox1Change');
  FileListBox1.Mask := FilterComboBox1.Mask;
  FileListBox1.Refresh;
  LoadFirstItem;
end;

procedure TForm1.FileListBox1Click(Sender: TObject);
begin
  DebugLn('FileListBox1Click');
  Memo1.Lines.LoadFromFile(FileListBox1.FileName);
  Caption := FileListBox1.FileName;
end;

procedure TForm1.DirectoryEdit1Change(Sender: TObject);
begin
  DebugLn('DirectoryEdit1Change');
  Hint := DirectoryEdit1.Directory;
  ChangeDirectory(DirectoryEdit1.Directory);
end;

end.

