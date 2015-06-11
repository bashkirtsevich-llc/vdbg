object FormCreateIso: TFormCreateIso
  Left = 215
  Top = 156
  BorderStyle = bsDialog
  Caption = #1057#1086#1079#1076#1072#1085#1080#1077' '#1086#1073#1088#1072#1079#1072' '#1076#1080#1089#1082#1072
  ClientHeight = 112
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 136
    Top = 16
    Width = 104
    Height = 13
    Caption = #1055#1091#1090#1100' '#1082' '#1086#1073#1088#1072#1079#1091' '#1076#1080#1089#1082#1072
  end
  object lblTot: TLabel
    Left = 312
    Top = 4
    Width = 9
    Height = 13
    Caption = '...'
  end
  object rdDisk: TRadioGroup
    Left = 20
    Top = 8
    Width = 109
    Height = 97
    Caption = #1059#1082#1072#1078#1080#1090#1077' '#1087#1088#1080#1074#1086#1076
    TabOrder = 1
  end
  object edIso: TEdit
    Left = 136
    Top = 36
    Width = 253
    Height = 21
    TabOrder = 0
  end
  object cmRip: TButton
    Left = 228
    Top = 76
    Width = 62
    Height = 29
    Caption = #1047#1072#1087#1080#1089#1100
    TabOrder = 2
    OnClick = cmRipClick
  end
  object cmClose: TButton
    Left = 297
    Top = 76
    Width = 93
    Height = 29
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 3
    OnClick = cmCloseClick
  end
  object cmChooseFile: TButton
    Left = 392
    Top = 36
    Width = 33
    Height = 21
    Caption = '...'
    TabOrder = 4
    OnClick = cmChooseFileClick
  end
  object cmStop: TButton
    Left = 156
    Top = 76
    Width = 62
    Height = 29
    Caption = #1054#1089#1090#1072#1085#1086#1074
    Enabled = False
    TabOrder = 5
    OnClick = cmStopClick
  end
  object saveRip: TSaveDialog
    DefaultExt = 'ISO'
    Filter = 'Iso files|*.iso|All files|*.*'
    Left = 328
    Top = 65528
  end
end
