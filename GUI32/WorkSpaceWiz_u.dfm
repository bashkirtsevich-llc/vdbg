object WorkSpaceWizform: TWorkSpaceWizform
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Workspace wiz'
  ClientHeight = 505
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    689
    505)
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    Left = 8
    Top = 8
    Width = 673
    Height = 458
    ActivePage = tsGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = #1054#1089#1085#1086#1074#1085#1086#1077
      DesignSize = (
        665
        430)
      object grpFile: TGroupBox
        Left = 3
        Top = 3
        Width = 656
        Height = 424
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = #1060#1072#1081#1083
        TabOrder = 0
        DesignSize = (
          656
          424)
        object lblComment: TLabel
          Left = 11
          Top = 63
          Width = 67
          Height = 13
          Caption = #1050#1086#1084#1084#1077#1085#1090#1072#1088#1080#1080
        end
        object edtWorkspaceName: TLabeledEdit
          Left = 11
          Top = 36
          Width = 608
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 54
          EditLabel.Height = 13
          EditLabel.Caption = #1048#1084#1103' '#1092#1072#1081#1083#1072
          TabOrder = 0
        end
        object btnWorkSpaceFName: TButton
          Left = 625
          Top = 36
          Width = 18
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = btnWorkSpaceFNameClick
        end
        object mmoComments: TMemo
          Left = 11
          Top = 82
          Width = 632
          Height = 329
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 2
        end
      end
    end
    object tsMemory: TTabSheet
      Caption = #1055#1072#1084#1103#1090#1100
      ImageIndex = 5
      DesignSize = (
        665
        430)
      object grpMemSize: TGroupBox
        Left = 3
        Top = 3
        Width = 656
        Height = 62
        Anchors = [akLeft, akTop, akRight]
        Caption = #1054#1073#1098#1077#1084' '#1054#1047#1059
        TabOrder = 0
        DesignSize = (
          656
          62)
        object lblMB: TLabel
          Left = 624
          Top = 27
          Width = 14
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'MB'
          ExplicitLeft = 326
        end
        object tbMemSize: TTrackBar
          Left = 11
          Top = 24
          Width = 560
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Max = 1024
          Min = 1
          PageSize = 64
          Frequency = 64
          Position = 1
          TabOrder = 0
          ThumbLength = 15
          OnChange = tbMemSizeChange
        end
        object edtSize: TEdit
          Left = 577
          Top = 24
          Width = 41
          Height = 21
          Anchors = [akTop, akRight]
          TabOrder = 1
          Text = '1'
        end
      end
      object grpBIOS: TGroupBox
        Left = 3
        Top = 71
        Width = 656
        Height = 356
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = #1053#1072#1095#1072#1083#1100#1085#1072#1103' '#1079#1072#1075#1088#1091#1079#1082#1072
        TabOrder = 1
        DesignSize = (
          656
          356)
        object lvModules: TListView
          Left = 11
          Top = 24
          Width = 633
          Height = 280
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
              Caption = #1048#1084#1103' '#1092#1072#1081#1083#1072
              Width = 500
            end
            item
              Caption = #1040#1076#1088#1077#1089
              Width = 100
            end>
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
        object btnMemAddFile: TButton
          Left = 585
          Top = 318
          Width = 27
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '+'
          TabOrder = 1
          OnClick = btnMemAddFileClick
        end
        object btnMemDelFile: TButton
          Left = 617
          Top = 318
          Width = 27
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '-'
          TabOrder = 2
          OnClick = btnMemDelFileClick
        end
      end
    end
    object tsCPU: TTabSheet
      Caption = #1062#1055
      ImageIndex = 1
      DesignSize = (
        665
        430)
      object grpRegState: TGroupBox
        Left = 3
        Top = 3
        Width = 262
        Height = 420
        Anchors = [akLeft, akTop, akBottom]
        Caption = #1053#1072#1095#1072#1083#1100#1085#1086#1077' '#1089#1086#1089#1090#1086#1103#1085#1080#1077' '#1088#1077#1075#1080#1089#1090#1088#1086#1074
        TabOrder = 0
        DesignSize = (
          262
          420)
        object sgridRegisters: TStringGrid
          Left = 10
          Top = 16
          Width = 241
          Height = 391
          Anchors = [akLeft, akTop, akRight, akBottom]
          ColCount = 2
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
          TabOrder = 0
          ColWidths = (
            64
            142)
        end
      end
      object grpCPUConf: TGroupBox
        Left = 271
        Top = 3
        Width = 386
        Height = 420
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = #1055#1088#1086#1094#1077#1089#1089#1086#1088
        TabOrder = 1
        DesignSize = (
          386
          420)
        object lblSpeed: TLabel
          Left = 10
          Top = 59
          Width = 42
          Height = 13
          Caption = #1063#1072#1089#1090#1086#1090#1072
        end
        object edtCPUName: TLabeledEdit
          Left = 10
          Top = 32
          Width = 363
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 19
          EditLabel.Height = 13
          EditLabel.Caption = #1048#1084#1103
          TabOrder = 0
        end
        object tbCPUSpeed: TTrackBar
          Left = 10
          Top = 78
          Width = 291
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Max = 500000
          Min = 10
          PageSize = 100
          Frequency = 10000
          Position = 10
          TabOrder = 1
          ThumbLength = 15
          OnChange = tbCPUSpeedChange
        end
        object edtCPUSpeed: TEdit
          Left = 307
          Top = 81
          Width = 66
          Height = 21
          Anchors = [akTop, akRight]
          TabOrder = 2
        end
        object chkFPU: TCheckBox
          Left = 10
          Top = 108
          Width = 363
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = #1055#1086#1076#1076#1077#1088#1078#1082#1072' '#1084#1072#1090'. '#1089#1086#1087#1088#1086#1094#1077#1089#1089#1086#1088#1072
          TabOrder = 3
        end
      end
    end
    object tsVGA: TTabSheet
      Caption = #1042#1080#1076#1077#1086
      ImageIndex = 5
      DesignSize = (
        665
        430)
      object grpVGAFont: TGroupBox
        Left = 3
        Top = 3
        Width = 654
        Height = 70
        Anchors = [akLeft, akTop, akRight]
        Caption = #1064#1088#1080#1092#1090#1099
        TabOrder = 0
        DesignSize = (
          654
          70)
        object edtVGAFont: TLabeledEdit
          Left = 11
          Top = 32
          Width = 599
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 75
          EditLabel.Height = 13
          EditLabel.Caption = #1060#1072#1081#1083' '#1096#1088#1080#1092#1090#1086#1074
          TabOrder = 0
        end
        object btnBrowsFont: TButton
          Left = 616
          Top = 32
          Width = 25
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = btnBrowsFontClick
        end
      end
    end
    object tsDisks: TTabSheet
      Caption = #1059#1089#1090#1088#1086#1081#1089#1090#1074#1072
      ImageIndex = 4
      DesignSize = (
        665
        430)
      object grpDrives: TGroupBox
        Left = 3
        Top = 3
        Width = 654
        Height = 424
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = #1059#1089#1090#1088#1086#1081#1089#1090#1074#1072
        TabOrder = 0
        DesignSize = (
          654
          424)
        object lstDriveTypes: TListBox
          Left = 11
          Top = 21
          Width = 126
          Height = 390
          Anchors = [akLeft, akTop, akBottom]
          ItemHeight = 13
          Items.Strings = (
            '(A:) FDD'
            '(B:) FDD'
            '(C:) HDD'
            '(D:) CDROM')
          TabOrder = 0
          OnClick = lstDriveTypesClick
        end
        object pnlDriveConf: TPanel
          Left = 143
          Top = 21
          Width = 498
          Height = 390
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvLowered
          TabOrder = 1
          DesignSize = (
            498
            390)
          object chkReadOnly: TCheckBox
            Left = 8
            Top = 75
            Width = 483
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = #1058#1086#1083#1100#1082#1086' '#1076#1083#1103' '#1095#1090#1077#1085#1080#1103
            Enabled = False
            TabOrder = 0
          end
          object lbledtDriveFileName: TLabeledEdit
            Left = 8
            Top = 48
            Width = 450
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 64
            EditLabel.Height = 13
            EditLabel.Caption = #1060#1072#1081#1083' '#1086#1073#1088#1072#1079#1072
            Enabled = False
            TabOrder = 1
          end
          object chkMounted: TCheckBox
            Left = 8
            Top = 8
            Width = 483
            Height = 17
            Caption = #1040#1082#1090#1080#1074#1077#1085
            TabOrder = 2
            OnClick = chkMountedClick
          end
          object grpHDDConf: TGroupBox
            Left = 8
            Top = 104
            Width = 483
            Height = 169
            Anchors = [akLeft, akTop, akRight]
            Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072' HDD'
            TabOrder = 3
            Visible = False
            DesignSize = (
              483
              169)
            object lbledtCyl: TLabeledEdit
              Left = 16
              Top = 32
              Width = 449
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 53
              EditLabel.Height = 13
              EditLabel.Caption = #1062#1080#1083#1080#1085#1076#1088#1099
              TabOrder = 0
            end
            object lbledtHeads: TLabeledEdit
              Left = 16
              Top = 81
              Width = 449
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 42
              EditLabel.Height = 13
              EditLabel.Caption = #1043#1086#1083#1086#1074#1082#1080
              TabOrder = 1
            end
            object lbledtSectors: TLabeledEdit
              Left = 16
              Top = 128
              Width = 449
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 45
              EditLabel.Height = 13
              EditLabel.Caption = #1057#1077#1082#1090#1086#1088#1099
              TabOrder = 2
            end
          end
          object btnApply: TButton
            Left = 416
            Top = 360
            Width = 75
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
            TabOrder = 4
            OnClick = btnApplyClick
          end
        end
      end
    end
    object tsBoot: TTabSheet
      Caption = #1047#1072#1075#1088#1091#1079#1082#1072
      ImageIndex = 3
      DesignSize = (
        665
        430)
      object grpBoot: TGroupBox
        Left = 3
        Top = 3
        Width = 654
        Height = 78
        Anchors = [akLeft, akTop, akRight]
        Caption = #1053#1072#1095#1072#1083#1100#1085#1072#1103' '#1079#1072#1075#1088#1091#1079#1082#1072
        TabOrder = 0
        DesignSize = (
          654
          78)
        object lblBoot: TLabel
          Left = 16
          Top = 21
          Width = 63
          Height = 13
          Caption = #1059#1089#1090#1088#1086#1081#1089#1090#1074#1086':'
        end
        object cbbBoot: TComboBox
          Left = 16
          Top = 40
          Width = 625
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Items.Strings = (
            'FDD'
            'HDD'
            'CDROM')
        end
      end
    end
  end
  object btnCancel: TButton
    Left = 606
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 2
  end
  object btnOk: TButton
    Left = 525
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object dlgSaveWorkSpace: TSaveDialog
    Left = 16
    Top = 472
  end
  object dlgOpenVGAFont: TOpenDialog
    Filter = 'Data files (*.dat)|*.dat'
    Left = 56
    Top = 472
  end
end
