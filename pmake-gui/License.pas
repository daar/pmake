unit License;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ButtonPanel;

type

  { TLicenseForm }

  TLicenseForm = class(TForm)
    ButtonPanel: TButtonPanel;
    LicenseMemo: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  LicenseForm: TLicenseForm;

implementation

{$R *.lfm}

end.

