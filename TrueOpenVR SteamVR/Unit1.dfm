object Main: TMain
  Left = 190
  Top = 123
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TrueOpenVR SteamVR'
  ClientHeight = 92
  ClientWidth = 292
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
  object ApplyBtn: TButton
    Left = 8
    Top = 59
    Width = 75
    Height = 25
    Caption = 'Install'
    TabOrder = 1
    OnClick = ApplyBtnClick
  end
  object CancelBtn: TButton
    Left = 88
    Top = 59
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelBtnClick
  end
  object AboutBtn: TButton
    Left = 256
    Top = 59
    Width = 28
    Height = 25
    Caption = '?'
    TabOrder = 3
    OnClick = AboutBtnClick
  end
  object DbgMdCb: TCheckBox
    Left = 224
    Top = 8
    Width = 60
    Height = 17
    Caption = 'Activate'
    TabOrder = 0
  end
  object XPManifest: TXPManifest
    Left = 224
    Top = 59
  end
end
