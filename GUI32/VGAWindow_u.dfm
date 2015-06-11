object VGAform: TVGAform
  Left = 0
  Top = 0
  Caption = 'VGA'
  ClientHeight = 337
  ClientWidth = 706
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'Lucida Console'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 11
  object pnlMemory: TPanel
    Left = 0
    Top = 0
    Width = 706
    Height = 28
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitWidth = 635
    DesignSize = (
      706
      28)
    object lblMemory: TLabel
      Left = 8
      Top = 6
      Width = 41
      Height = 13
      Caption = #1055#1072#1084#1103#1090#1100':'
    end
    object cbbMemory: TComboBox
      Left = 55
      Top = 3
      Width = 305
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = cbbMemoryChange
      Items.Strings = (
        'VGA'
        'VBE'
        'Text')
    end
    object edtxResolution: TLabeledEdit
      Left = 413
      Top = 3
      Width = 121
      Height = 21
      Anchors = [akTop, akRight]
      EditLabel.Width = 40
      EditLabel.Height = 13
      EditLabel.Caption = #1064#1080#1088#1080#1085#1072
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 1
    end
    object edtyresolution: TLabeledEdit
      Left = 581
      Top = 3
      Width = 121
      Height = 21
      Anchors = [akTop, akRight]
      EditLabel.Width = 37
      EditLabel.Height = 13
      EditLabel.Caption = #1042#1099#1089#1086#1090#1072
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 2
    end
  end
  object actlstMain: TActionList
    Left = 8
    Top = 32
    object actGotoOffset: TAction
      Caption = 'actGotoOffset'
      ShortCut = 16455
      OnExecute = actGotoOffsetExecute
    end
  end
end
