unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, XPMan, Registry;

type
  TMain = class(TForm)
    InstallBtn: TButton;
    CancelBtn: TButton;
    AboutBtn: TButton;
    XPManifest: TXPManifest;
    DbgMdCb: TCheckBox;
    DbgMdLbl: TLabel;
    UninstallBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure UninstallBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;
  SteamPath: string;

implementation

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
var
  Reg: TRegistry;
begin
  Application.Title:=Caption;

  Reg:=TRegistry.Create;
  //Steam
  Reg.RootKey:=HKEY_CURRENT_USER;
  if (Reg.OpenKey('\Software\Valve\Steam', false)) then
    SteamPath:=StringReplace(Reg.ReadString('SteamPath'), '/', '\', [rfReplaceAll]);
  Reg.CloseKey;
  Reg.Free;

  DbgMdLbl.Caption:=DbgMdLbl.Caption + #13#10 + 'Windowed borderless fullscreen' + #13#10 + 'with lock to 30 FPS';
end;

procedure TMain.AboutBtnClick(Sender: TObject);
begin
  Application.MessageBox('TrueOpenVR SteamVR' + #13#10 +
  'https://github.com/TrueOpenVR' + #13#10 +
  'r57zone@gmail.com', PChar(Caption), MB_ICONINFORMATION);
end;

procedure TMain.InstallBtnClick(Sender: TObject);
var
  Reg: TRegistry;
  Config: TStringList;
  Error: boolean;

  RenderWidth, RenderHeight, ScreenIndex: integer;
  IPD, DistortionK1, DistortionK2: double;
begin
  Error:=false;

  Reg:=TRegistry.Create;
  //TrueOpenVR
  Reg.RootKey:=HKEY_CURRENT_USER;
  if Reg.OpenKey('\Software\TrueOpenVR', false) then begin
    try
      ScreenIndex:=Reg.ReadInteger('ScreenIndex');
      ScreenIndex:=ScreenIndex - 1;
      RenderWidth:=Reg.ReadInteger('RenderWidth');
      RenderHeight:=Reg.ReadInteger('RenderHeight');
      IPD:=Reg.ReadFloat('IPD');
      DistortionK1:=Reg.ReadFloat('DistortionK1');
      DistortionK2:=Reg.ReadFloat('DistortionK2');
    except
      Error:=true;
    end;
  end else
    Error:=true;

  Reg.CloseKey;

  Reg.Free;

  if Error then begin
    Application.MessageBox('TrueOpenVR not found. Please install and try again.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  if ScreenIndex > Screen.MonitorCount - 1 then begin
    Application.MessageBox('VR screen not found. Please connect VR screen,' + #13#10 + 'restart program and try again.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  if DirectoryExists(SteamPath) then begin

    if FileExists(ExtractFilePath(ParamStr(0)) + 'OpenVR\default.vrsettings') then begin

      CreateDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr');
      CreateDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin');
      CreateDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin\win32');
      CreateDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin\win64');
      CreateDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\resources');
      CreateDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\resources\settings');

      Config:=TStringList.Create;
      Config.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'OpenVR\default.vrsettings');

      Config.Text:=StringReplace(Config.Text, '<RENDERWIDTH>', IntToStr(RenderWidth), [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<RENDERHEIGHT>', IntToStr(RenderHeight), [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<WINDOWWIDTH>', IntToStr(Screen.Monitors[ScreenIndex].Width), [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<WINDOWHEIGHT>', IntToStr(Screen.Monitors[ScreenIndex].Height), [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<WINDOWX>', IntToStr(Screen.Monitors[ScreenIndex].Left), [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<WINDOWY>', IntToStr(Screen.Monitors[ScreenIndex].Top), [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<DISTORTIONK1>', StringReplace(FloatToStr(DistortionK1), DecimalSeparator, '.', [rfReplaceAll]), [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<DISTORTIONK2>', StringReplace(FloatToStr(DistortionK2), DecimalSeparator, '.', [rfReplaceAll]), [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<IPD>', StringReplace(FloatToStr(IPD), DecimalSeparator, '.', [rfReplaceAll]), [rfReplaceAll]);

      if DbgMdCb.Checked then
        Config.Text:=StringReplace(Config.Text, '<DEBUGMODE>', 'true', [rfReplaceAll])
      else
        Config.Text:=StringReplace(Config.Text, '<DEBUGMODE>', 'false', [rfReplaceAll]);

      Config.SaveToFile(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\resources\settings\default.vrsettings');
      Config.Free;

    end else begin
      Application.MessageBox('File "default.vrsettings" not found.', PChar(Caption), MB_ICONERROR);
      Error:=true;
    end;

    if not ((CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\driver.vrdrivermanifest'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\driver.vrdrivermanifest'), false)) and
      (CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\bin\win32\driver_tovr.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin\win32\driver_tovr.dll'), false)) and
      (CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\bin\win64\driver_tovr.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin\win64\driver_tovr.dll'), false)))
       then begin
        Application.MessageBox('Error copy driver files. Please close Steam and SteamVR.', PChar(Caption), MB_ICONERROR);
        Error:=true;
      end;

    if Error = false then
      Application.MessageBox('Installed', PChar(Caption), MB_ICONINFORMATION);

  end else
    Application.MessageBox('Steam not found. Please install Steam and SteamVR', PChar(Caption), MB_ICONERROR);

  if Error = false then
    Close;
end;

procedure TMain.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.UninstallBtnClick(Sender: TObject);
begin
  if DirectoryExists(SteamPath) then begin
      DeleteFile(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\driver.vrdrivermanifest');
      DeleteFile(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin\win32\driver_tovr.dll');
      DeleteFile(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin\win64\driver_tovr.dll');
      RemoveDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin\win32');
      RemoveDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin\win64');
      RemoveDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\bin');

      DeleteFile(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\resources\settings\default.vrsettings');
      RemoveDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\resources\settings');
      RemoveDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr\resources');

      RemoveDir(SteamPath + '\steamapps\common\SteamVR\drivers\tovr');
      Application.MessageBox('Uninstalled', PChar(Caption), MB_ICONINFORMATION);
  end else
    Application.MessageBox('Steam not found. Please install Steam and SteamVR', PChar(Caption), MB_ICONERROR);
end;

end.
