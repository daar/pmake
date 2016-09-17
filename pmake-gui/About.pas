unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ExtCtrls, LCLIntf, Controls, StdCtrls, License;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CompilerLabel: TLabel;
    CreditsButton: TButton;
    LicenseButton: TButton;
    CloseButton: TButton;
    ApplicationLogoImage: TImage;
    ApplicationTitleLabel: TLabel;
    CopyrightLabel: TLabel;
    RepositoryLabel: TLabel;
    procedure CreditsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LicenseButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure RepositoryLabelClick(Sender: TObject);
    procedure RepositoryLabelMouseEnter(Sender: TObject);
    procedure RepositoryLabelMouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  SysUtils, ufmake, fpmkunit;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.RepositoryLabelClick(Sender: TObject);
begin
  OpenURL('https://github.com/daar/pmake');
end;

procedure TAboutForm.RepositoryLabelMouseEnter(Sender: TObject);
begin
  RepositoryLabel.Cursor := crHandPoint;
  RepositoryLabel.Font.Underline := True;
end;

procedure TAboutForm.RepositoryLabelMouseLeave(Sender: TObject);
begin
  RepositoryLabel.Cursor := crDefault;
  RepositoryLabel.Font.Underline := False;
end;

procedure TAboutForm.CreditsButtonClick(Sender: TObject);
begin
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  CompilerLabel.Caption := Format('Current compiler: FPC %s %s-%s', [CompilerVersion, CPUToString(CPU), OSToString(OS)]);
end;

procedure TAboutForm.LicenseButtonClick(Sender: TObject);
begin
  if LicenseForm = nil then
    LicenseForm := TLicenseForm.Create(nil);

  LicenseForm.ShowModal;

  LicenseForm.Free;
end;

end.
