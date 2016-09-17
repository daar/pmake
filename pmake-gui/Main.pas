unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, EditBtn, ButtonPanel, ExtCtrls, Menus, LCLTranslator, DefaultTranslator,
  ufmake;

type

  { TPMakeGUIForm }

  TPMakeGUIForm = class(TForm)
    BrowseSourceButton: TButton;
    BrowseBuildButton: TButton;
    ConfigureButton: TButton;
    GenerateButton: TButton;
    ButtonPanel: TButtonPanel;
    GroupedCheckBox: TCheckBox;
    BrowseSourceEdit: TEdit;
    BrowseBuildEdit: TEdit;
    LanguagesMenuItem: TMenuItem;
    EnglishMenuItem: TMenuItem;
    DutchMenuItem: TMenuItem;
    MenuItem8: TMenuItem;
    SourceTreeLocationLabel: TLabel;
    BinaryLocationLabel: TLabel;
    MainMenu: TMainMenu;
    MessagesMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    ConfigureMenuItem: TMenuItem;
    MenuItem9: TMenuItem;
    MessagePanel: TPanel;
    ConfigurePanel: TPanel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    Splitter: TSplitter;
    OptionsTreeView: TTreeView;
    procedure BrowseSourceButtonClick(Sender: TObject);
    procedure BrowseBuildButtonClick(Sender: TObject);
    procedure ConfigureButtonClick(Sender: TObject);
    procedure LanguagesMenuItemClick(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure ButtonPanelClick(Sender: TObject);
    procedure BrowseSourceEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PMakeGUIForm: TPMakeGUIForm;

implementation

{$R *.lfm}

uses
  About;

{ TPMakeGUIForm }

procedure TPMakeGUIForm.ButtonPanelClick(Sender: TObject);
begin
  Close;
end;

procedure TPMakeGUIForm.BrowseSourceEditChange(Sender: TObject);
var
  i: integer;
  BrowseSource: String;
begin
  //initialize default build folder
  BrowseSource := IncludeTrailingPathDelimiter(BrowseSourceEdit.Text);
  BrowseBuildEdit.Text := ExpandFileName(BrowseSource + '..' + PathDelim + 'build');

  fmakefiles.Clear;
  search_fmake(BrowseSource);

  for i := 0 to fmakefiles.Count - 1 do
  begin
    OptionsTreeView.Items.Add(nil, fmakefiles[i]);
  end;
end;

procedure TPMakeGUIForm.BrowseSourceButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    BrowseSourceEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TPMakeGUIForm.BrowseBuildButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    BrowseBuildEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TPMakeGUIForm.ConfigureButtonClick(Sender: TObject);
begin

end;

procedure TPMakeGUIForm.GenerateButtonClick(Sender: TObject);
begin

end;

procedure TPMakeGUIForm.LanguagesMenuItemClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: SetDefaultLang('en');
    1: SetDefaultLang('nl');
  end;
end;

procedure TPMakeGUIForm.FormCreate(Sender: TObject);
//var
//  menuItem : TMenuItem;
begin
  Caption := Format('PMake %s', [FMakeVersion]);

  //menuItem := TMenuItem.Create(LanguagesMenuItem) ;
  //menuItem.Caption := 'Dutch';
  //menuItem.OnClick := @LanguagesMenuItemClick;
  //menuItem.Tag := 1;
  //LanguagesMenuItem.Add(menuItem) ;

  ButtonPanel.CloseButton.Cancel := True;

  //clear the GUI
  BrowseSourceEdit.Text := '';
  BrowseBuildEdit.Text := '';
  OptionsTreeView.Items.Clear;
  MessagesMemo.Lines.Clear;
end;

procedure TPMakeGUIForm.MenuItem18Click(Sender: TObject);
begin
  if AboutForm = nil then
    AboutForm := TAboutForm.Create(nil);

  AboutForm.ShowModal;

  FreeAndNil(AboutForm);
end;

procedure TPMakeGUIForm.MenuItem1Click(Sender: TObject);
begin

end;

procedure TPMakeGUIForm.MenuItem5Click(Sender: TObject);
begin
  Close;
end;

end.

