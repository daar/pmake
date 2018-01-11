unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, EditBtn, ButtonPanel, ExtCtrls, Menus, LCLTranslator, DefaultTranslator;

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
    procedure FormCreate(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    function CheckAndCreateDirectory(ADirectory: string): boolean;
    { private declarations }
  public
    { public declarations }
  end;

var
  PMakeGUIForm: TPMakeGUIForm;

implementation

{$R *.lfm}

uses
  About, pmake_variables, make_main, pmake_utilities, pmake_main;

procedure GUIOutputLn(msg: string);
begin
  PMakeGUIForm.MessagesMemo.Lines.Add(msg);
end;

{ TPMakeGUIForm }

procedure TPMakeGUIForm.ButtonPanelClick(Sender: TObject);
begin
  Close;
end;

procedure TPMakeGUIForm.BrowseSourceButtonClick(Sender: TObject);
var
  BrowseSource: string;
begin
  if SelectDirectoryDialog.Execute then
  begin
    ConfigureButton.Enabled := True;
    BrowseSource := IncludeTrailingPathDelimiter(SelectDirectoryDialog.FileName);

    BrowseSourceEdit.Text := BrowseSource;

    //initialize default build folder
    BrowseBuildEdit.Text := IncludeTrailingPathDelimiter(ExpandFileName(BrowseSource + '..' + PathDelim + 'build'));
  end;
end;

procedure TPMakeGUIForm.BrowseBuildButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    BrowseBuildEdit.Text := IncludeTrailingPathDelimiter(SelectDirectoryDialog.FileName);
end;

procedure TPMakeGUIForm.ConfigureButtonClick(Sender: TObject);
begin
  if not DirectoryExists(BrowseSourceEdit.Text) then
  begin
    ShowMessage('Cannot find source directory, please specify.');
    exit;
  end;
  if not CheckAndCreateDirectory(BrowseBuildEdit.Text) then
     //folder does not exists
    exit;

  GenerateButton.Enabled := True;

  pmake_initialize(BrowseBuildEdit.Text);

  set_('PMAKE_SOURCE_DIR', BrowseSourceEdit.Text);

  pmakecache_write;
end;

procedure TPMakeGUIForm.GenerateButtonClick(Sender: TObject);
begin
  create_and_build_make;
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
//  menuItem: TMenuItem;
begin
  OutputLn := @GUIOutputLn;
  StdOutLn := @GUIOutputLn;
  StdErrLn := @GUIOutputLn;

  Caption := Format('PMake %s', [PMAKE_VERSION]);

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

function TPMakeGUIForm.CheckAndCreateDirectory(ADirectory: string): boolean;
begin
  Result := True;

  if not DirectoryExists(ADirectory) then
  begin
    if MessageDlg('Create directory',
        Format(
          'The directory %s does not exist.%sWould you like to create it?', [
          BrowseBuildEdit.Text, LineEnding]),
        mtInformation, [mbYes, mbNo], 0) = mrYes
    then
    begin
      if not CreateDir(BrowseBuildEdit.Text) then
      begin
        OutputLn(Format('Failed to create directory %s?', [BrowseBuildEdit.Text]));
        Result := False;
      end
      else
        Result := True
    end
    else
      Result := False;
  end
  else
    Result := True;
end;

end.

