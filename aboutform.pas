
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
    btnClose: TBitBtn;
    imgLogo: TImage;
    lblWebsite: TLabel;
    lblTitle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
    procedure lblURLMouseEnter(Sender: TObject);
    procedure lblURLMouseLeave(Sender: TObject);
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
  imgLogo.Picture.Assign(Application.Icon);
  imgLogo.Picture.Icon.Current := 0;
end;

procedure TAboutForm.lblURLClick(Sender: TObject);
begin
  if Sender = lblWebsite then
    OpenURL('https://github.com/rchastain/textview');
end;

procedure TAboutForm.lblURLMouseEnter(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style + [fsUnderline];
end;

procedure TAboutForm.lblURLMouseLeave(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style - [fsUnderline];
end;

end.

