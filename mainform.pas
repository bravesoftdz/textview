unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ShellCtrls, FileCtrl, StdCtrls, EditBtn, ComCtrls, SynEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    DirectoryEdit1: TDirectoryEdit;
    FileListBox1: TFileListBox;
    FilterComboBox1: TFilterComboBox;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ShellTreeView1: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Memo1: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure FileListBox1Click(Sender: TObject);
    procedure FilterComboBox1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure ShellTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
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
    DirectoryEdit1.Directory := LDirectory
  else
    DirectoryEdit1.Directory :=
{$ifdef mswindows}
      string(GetWindowsSpecialDir(CSIDL_PERSONAL, FALSE))
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
  Memo1.Font.Height := LFontHeight;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DebugLn('FormClose');
  SaveSettings(DirectoryEdit1.Directory, FilterComboBox1.Filter, FilterComboBox1.ItemIndex, Memo1.Font.Height);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
var
  F: TAboutForm;
begin
  F := TAboutForm.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TForm1.ShellTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  DebugLn('ShellTreeView1Change');
  ChangeDirectory(ExcludeTrailingPathDelimiter(ShellTreeView1.Path));
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  if Memo1.Font.Height > 13 then Memo1.Font.Height := Pred(Memo1.Font.Height);
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  if Memo1.Font.Height < 18 then Memo1.Font.Height := Succ(Memo1.Font.Height);
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
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

procedure TForm1.LoadFile(const AFileName: TFileName);
var
  LList: TStringList;
  LEncoding: string;
begin
  DebugLn(Format('LoadFile(%s)', [AFileName]));

  if AFileName = FLastFileLoaded then
  begin
    DebugLn('File already loaded');
    Exit;
  end else
    FLastFileLoaded := AFileName;

  if MenuItem4.Checked then
  begin
    LList := TStringList.Create;
    LList.LoadFromFile(AFileName);
    LEncoding := GuessEncoding(LList.Text);
    if LEncoding = 'utf8' then
      Memo1.Lines.Text := LList.Text
    else
    begin
      DebugLn(Format('Conversion du texte en UTF-8 : %s', [AFileName]));
      DebugLn(Format('Encodage initial : %s', [LEncoding]));
      Memo1.Lines.Text := ConvertEncoding(LList.Text, LEncoding, EncodingUTF8);
    end;
    LList.Free;
  end else
    Memo1.Lines.LoadFromFile(AFileName);
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
  //Memo1.Lines.LoadFromFile(FileListBox1.FileName);
  LoadFile(FileListBox1.FileName);
  Caption := FileListBox1.FileName;
end;

procedure TForm1.DirectoryEdit1Change(Sender: TObject);
begin
  DebugLn('DirectoryEdit1Change');
  Hint := DirectoryEdit1.Directory;
  ChangeDirectory(DirectoryEdit1.Directory);
end;

end.

