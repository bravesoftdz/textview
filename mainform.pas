unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ShellCtrls, FileCtrl, StdCtrls, EditBtn, ComCtrls, SynEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    edDirectory: TDirectoryEdit;
    lbFiles: TFileListBox;
    cbFilter: TFilterComboBox;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    miTextView: TMenuItem;
    miQuit: TMenuItem;
    miOptions: TMenuItem;
    miConvert: TMenuItem;
    miHelp: TMenuItem;
    miAbout: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    tvDirectories: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    seText: TSynEdit;
    ToolBar1: TToolBar;
    tbDecrease: TToolButton;
    tbIncrease: TToolButton;
    tbQuit: TToolButton;
    tbNewInstance: TToolButton;
    procedure edDirectoryChange(Sender: TObject);
    procedure lbFilesClick(Sender: TObject);
    procedure cbFilterChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure miQuitClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure tvDirectoriesChange(Sender: TObject; Node: TTreeNode);
    procedure tbDecreaseClick(Sender: TObject);
    procedure tbIncreaseClick(Sender: TObject);
    procedure tbNewInstanceClick(Sender: TObject);
  private
    FLastFileLoaded: TFileName;
    procedure ChangeDirectory(const ADirectory: string);
    procedure LoadFirstItem;
    procedure LoadFile(const AFileName: TFileName);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Settings,
{$ifdef mswindows}
  WinDirs,
{$endif}
  LazLogger,
  LConvEncoding,
  AboutForm;

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
var
  LDirectory, LFilter: string;
  LItemIndex, LFontHeight: integer;
begin
  DebugLn('FormActivate');
  DebugLn(Format('ParamStr(0) = %s', [ParamStr(0)]));
  DebugLn(Format('Application.ExeName = %s', [Application.ExeName]));
  FLastFileLoaded := '';
  Caption := 'TextView ' + {$I version.inc};
  Splitter1.Height := Self.ClientHeight - 2 * 8 - ToolBar1.Height;
  LoadSettings(LDirectory, LFilter, LItemIndex, LFontHeight);
  if DirectoryExists(LDirectory) then
    edDirectory.Directory := LDirectory
  else
    edDirectory.Directory :=
{$ifdef mswindows}
      string(GetWindowsSpecialDir(CSIDL_PERSONAL, FALSE))
{$else}
      GetEnvironmentVariable('HOME')
{$endif}
    ;
  if Length(LFilter) > 0 then
    cbFilter.Filter := LFilter;
  if LItemIndex >= 0 then
  begin
    cbFilter.ItemIndex := LItemIndex;
    cbFilterChange(Sender);
  end;
  seText.Font.Height := LFontHeight;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DebugLn('FormClose');
  SaveSettings(edDirectory.Directory, cbFilter.Filter, cbFilter.ItemIndex, seText.Font.Height);
end;

procedure TForm1.miQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.miAboutClick(Sender: TObject);
var
  LForm: TAboutForm;
begin
  LForm := TAboutForm.Create(nil);
  try
    LForm.ShowModal;
  finally
    LForm.Free;
  end;
end;

procedure TForm1.tvDirectoriesChange(Sender: TObject; Node: TTreeNode);
begin
  DebugLn('ShellTreeView1Change');
  ChangeDirectory(ExcludeTrailingPathDelimiter(tvDirectories.Path));
end;

procedure TForm1.tbDecreaseClick(Sender: TObject);
begin
  if seText.Font.Height > 13 then seText.Font.Height := Pred(seText.Font.Height);
end;

procedure TForm1.tbIncreaseClick(Sender: TObject);
begin
  if seText.Font.Height < 18 then seText.Font.Height := Succ(seText.Font.Height);
end;

procedure TForm1.tbNewInstanceClick(Sender: TObject);
begin
  try
    ExecuteProcess(Application.ExeName, [], []);
  except
    on E: EOSError do
      DebugLn(Format('Error %d: %s', [E.ErrorCode, E.Message]));
  end;
end;

procedure TForm1.ChangeDirectory(const ADirectory: string);
begin
  DebugLn(Format('ChangeDirectory(%s)', [ADirectory]));
  if edDirectory.Directory <> ADirectory then edDirectory.Directory := ADirectory;
  if ExcludeTrailingPathDelimiter(tvDirectories.Path) <> ADirectory then tvDirectories.Path := IncludeTrailingPathDelimiter(ADirectory);
  if lbFiles.Directory <> ADirectory then
  begin
    lbFiles.Directory := ADirectory;
    LoadFirstItem;
  end;
end;

procedure TForm1.LoadFirstItem;
begin
  DebugLn('LoadFirstItem');
  if lbFiles.Items.Count > 0 then
  begin
    lbFiles.ItemIndex := 0;
    lbFiles.Click;
  end else
    seText.Clear;
end;

procedure TForm1.LoadFile(const AFileName: TFileName);
var
  LList: TStringList;
  LEncoding: string;
begin
  DebugLn(Format('LoadFile(%s)', [AFileName]));

  if AFileName = FLastFileLoaded then
  begin
    DebugLn('Exit');
    Exit;
  end else
    FLastFileLoaded := AFileName;

  if miConvert.Checked then
  begin
    LList := TStringList.Create;
    LList.LoadFromFile(AFileName);
    LEncoding := GuessEncoding(LList.Text);
    if LEncoding = 'utf8' then
      seText.Lines.Text := LList.Text
    else
    begin
      DebugLn(Format('Converting to UTF-8: %s', [AFileName]));
      DebugLn(Format('File encoding: %s', [LEncoding]));
      seText.Lines.Text := ConvertEncoding(LList.Text, LEncoding, EncodingUTF8);
    end;
    LList.Free;
  end else
    seText.Lines.LoadFromFile(AFileName);
end;

procedure TForm1.cbFilterChange(Sender: TObject);
begin
  DebugLn('FilterComboBox1Change');
  lbFiles.Mask := cbFilter.Mask;
  lbFiles.Refresh;
  LoadFirstItem;
end;

procedure TForm1.lbFilesClick(Sender: TObject);
begin
  DebugLn('FileListBox1Click');
  LoadFile(lbFiles.FileName);
  Caption := lbFiles.FileName;
end;

procedure TForm1.edDirectoryChange(Sender: TObject);
begin
  DebugLn('DirectoryEdit1Change');
  Hint := edDirectory.Directory;
  ChangeDirectory(edDirectory.Directory);
end;

end.

