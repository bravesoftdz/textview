
(* https://github.com/wp-xyz/Hex *)

unit aboutform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btClose: TBitBtn;
    imLogo: TImage;
    lbWebsite: TLabel;
    lbTitle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure URLClick(Sender: TObject);
    procedure URLMouseEnter(Sender: TObject);
    procedure URLMouseLeave(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

uses
  LCLIntf;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  imLogo.Picture.Assign(Application.Icon);
  imLogo.Picture.Icon.Current := 0;
  lbTitle.Caption := 'TextView ' + {$I version.inc};
end;

procedure TAboutForm.URLClick(Sender: TObject);
begin
  if Sender = lbWebsite then
    OpenURL('https://github.com/rchastain/textview');
end;

procedure TAboutForm.URLMouseEnter(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style + [fsUnderline];
end;

procedure TAboutForm.URLMouseLeave(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style - [fsUnderline];
end;

end.

