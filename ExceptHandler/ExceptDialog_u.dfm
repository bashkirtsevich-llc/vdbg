object ExceptionDialog: TExceptionDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'System fault'
  ClientHeight = 417
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    537
    417)
  PixelsPerInch = 96
  TextHeight = 13
  object imgErrorIcon: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
  end
  object lblExceptionmessage: TLabel
    Left = 52
    Top = 19
    Width = 173
    Height = 13
    Caption = 'An error occupied in the application.'
  end
  object lblCopyright: TLabel
    Left = 8
    Top = 397
    Width = 161
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Copyright (c) 2012 M.A.D.M.A.N.'
    Enabled = False
  end
  object btnContinue: TButton
    Tag = 1
    Left = 454
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Continue'
    TabOrder = 0
    OnClick = OnBtnsClick
  end
  object btnRestart: TButton
    Tag = 2
    Left = 454
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Restart'
    TabOrder = 1
    OnClick = OnBtnsClick
  end
  object btnClose: TButton
    Tag = 3
    Left = 454
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = OnBtnsClick
  end
  object btnSendReport: TButton
    Tag = 4
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Send report'
    TabOrder = 3
    OnClick = OnBtnsClick
  end
  object btnDetails: TButton
    Left = 89
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Details'
    TabOrder = 4
    OnClick = btnDetailsClick
  end
  object pgcMain: TPageControl
    Left = 8
    Top = 95
    Width = 521
    Height = 298
    ActivePage = tsGeneral
    TabOrder = 5
    object tsGeneral: TTabSheet
      Caption = 'General'
      object lvGeneral: TListView
        Left = 0
        Top = 0
        Width = 513
        Height = 270
        Align = alClient
        Columns = <
          item
            Caption = 'Description'
            Width = 150
          end
          item
            Caption = 'Value'
            Width = 340
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsThreads: TTabSheet
      Caption = 'Threads'
      ImageIndex = 4
      object lvThreads: TListView
        Left = 0
        Top = 0
        Width = 513
        Height = 270
        Align = alClient
        Columns = <
          item
            Caption = 'Thread info'
            Width = 480
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsModules: TTabSheet
      Caption = 'Modules'
      ImageIndex = 1
      object lvModules: TListView
        Left = 0
        Top = 0
        Width = 513
        Height = 270
        Align = alClient
        Columns = <
          item
            Caption = 'Module name'
            Width = 170
          end
          item
            Caption = 'Path'
            Width = 310
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsTasks: TTabSheet
      Caption = 'Tasks'
      ImageIndex = 2
      object lvTasks: TListView
        Left = 0
        Top = 0
        Width = 513
        Height = 270
        Align = alClient
        Columns = <
          item
            Caption = 'Task'
            Width = 480
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsProcesses: TTabSheet
      Caption = 'Processes'
      ImageIndex = 3
      object lvProcesses: TListView
        Left = 0
        Top = 0
        Width = 513
        Height = 270
        Align = alClient
        Columns = <
          item
            Caption = 'Process name'
            Width = 170
          end
          item
            Caption = 'Path'
            Width = 300
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsStackTrace: TTabSheet
      Caption = 'Stack trace'
      ImageIndex = 5
      object lvStackTrace: TListView
        Left = 0
        Top = 0
        Width = 513
        Height = 270
        Align = alClient
        Columns = <
          item
            Caption = 'Address'
            Width = 80
          end
          item
            Caption = 'Info'
            Width = 400
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsDump: TTabSheet
      Caption = 'Dump'
      ImageIndex = 6
      object mmoDump: TMemo
        Left = 0
        Top = 0
        Width = 513
        Height = 270
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Lucida Console'
        Font.Style = []
        Lines.Strings = (
          'mmoDump')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
