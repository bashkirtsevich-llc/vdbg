object CPUform: TCPUform
  Left = 0
  Top = 0
  Caption = 'CPU'
  ClientHeight = 513
  ClientWidth = 825
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object splMiddle: TSplitter
    Left = 0
    Top = 321
    Width = 825
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 41
    ExplicitWidth = 472
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 825
    Height = 321
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object splCPURegistersAndFlags: TSplitter
      Left = 629
      Top = 0
      Height = 321
      Align = alRight
      ExplicitLeft = 536
      ExplicitTop = 112
      ExplicitHeight = 100
    end
    object pnlCPU: TPanel
      Left = 0
      Top = 0
      Width = 629
      Height = 321
      Align = alClient
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object pnlRegistersAndFlags: TPanel
      Left = 632
      Top = 0
      Width = 193
      Height = 321
      Align = alRight
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 324
    Width = 825
    Height = 189
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object splDumpStack: TSplitter
      Left = 629
      Top = 0
      Height = 189
      Align = alRight
      ExplicitLeft = 568
      ExplicitTop = 136
      ExplicitHeight = 100
    end
    object pnlMemoryDump: TPanel
      Left = 0
      Top = 0
      Width = 629
      Height = 189
      Align = alClient
      BevelOuter = bvNone
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object pnlStackTrace: TPanel
      Left = 632
      Top = 0
      Width = 193
      Height = 189
      Align = alRight
      BevelOuter = bvNone
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object actlstMain: TActionList
    Left = 16
    Top = 8
    object actGotoOffset: TAction
      Caption = 'actGotoOffset'
      ShortCut = 16455
      OnExecute = actGotoOffsetExecute
    end
  end
end
