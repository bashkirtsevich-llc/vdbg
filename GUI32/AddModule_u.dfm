object AddModuleform: TAddModuleform
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Add module'
  ClientHeight = 145
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  DesignSize = (
    313
    145)
  PixelsPerInch = 96
  TextHeight = 13
  object edtFileName: TLabeledEdit
    Left = 8
    Top = 24
    Width = 272
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 54
    EditLabel.Height = 13
    EditLabel.Caption = #1048#1084#1103' '#1092#1072#1081#1083#1072
    TabOrder = 0
  end
  object edtAddress: TLabeledEdit
    Left = 8
    Top = 64
    Width = 297
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    CharCase = ecUpperCase
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = #1040#1076#1088#1077#1089' (HEX)'
    TabOrder = 2
  end
  object btnOk: TButton
    Left = 152
    Top = 112
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 233
    Top = 112
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnBrows: TButton
    Left = 286
    Top = 24
    Width = 19
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
  end
  object dlgOpenModule: TOpenDialog
    Left = 8
    Top = 112
  end
end
