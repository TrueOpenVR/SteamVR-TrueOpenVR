object Main: TMain
  Left = 190
  Top = 123
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TrueOpenVR SteamVR'
  ClientHeight = 92
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DbgMdLbl: TLabel
    Left = 8
    Top = 8
    Width = 161
    Height = 13
    Caption = 'Debug mode (not recommended): '
  end
  object InstallBtn: TButton
    Left = 8
    Top = 59
    Width = 75
    Height = 25
    Caption = 'Install'
    TabOrder = 1
    OnClick = InstallBtnClick
  end
  object CancelBtn: TButton
    Left = 168
    Top = 59
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelBtnClick
  end
  object AboutBtn: TButton
    Left = 248
    Top = 59
    Width = 28
    Height = 25
    Caption = '?'
    TabOrder = 4
    OnClick = AboutBtnClick
  end
  object DbgMdCb: TCheckBox
    Left = 216
    Top = 8
    Width = 60
    Height = 17
    Caption = 'Activate'
    TabOrder = 0
  end
  object UninstallBtn: TButton
    Left = 88
    Top = 59
    Width = 75
    Height = 25
    Caption = 'Uninstall'
    TabOrder = 2
    OnClick = UninstallBtnClick
  end
  object XPManifest: TXPManifest
    Left = 216
    Top = 27
  end
end
