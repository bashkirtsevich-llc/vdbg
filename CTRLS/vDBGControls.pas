unit vDBGControls;

{$R-,T-,H+,X+}

interface

uses
  Messages, Windows, SysUtils, Classes, Variants,
  Types, Graphics, Menus, Controls, Forms, StdCtrls, Mask;

const
  MaxShortInt = High(ShortInt);
  MaxCustomExtents = MaxListSize;

type
  TValuesList = class
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    FLines: TStringList;
    FSelectedLine: integer;
    FCurrentLine: TStringList;
    FCurrentPos: integer;
    FMode: integer;
    FAdditionalLines: TStringList;

    procedure Change;
  public
    function AddLine(const aCaption: string;
      const aAdditionalCaption: string = ''): TValuesList; // ������� �������
    function AddValue(const aValue: string): TValuesList; // �������� �������� � ��������� �������

    function Select(const aCaption: string): TValuesList; overload;
    function Select(aIndex: integer): TValuesList; overload;
    function ValCount: integer;
    function GetValue(aIndex: integer): string; overload;
    function GetValue: string; overload;
    function Caption: string;
    function EOF: boolean;
    function Next: TValuesList;
    function UpdateValue(const aValue: string): TValuesList;
    function IsChanged: boolean;

    procedure Commit; // ��������� ���������

    function Count: integer;

    property Mode: integer read FMode write FMode;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    constructor Create(AOwner: TComponent);
    destructor Destroy;
  end;

type
  { Internal grid types }
  TGetExtentsFunc = function(Index: Longint): Integer of object;

  TGridAxisDrawInfo = record
    EffectiveLineWidth: Integer;
    FixedBoundary: Integer;
    GridBoundary: Integer;
    GridExtent: Integer;
    LastFullVisibleCell: Longint;
    FullVisBoundary: Integer;
    FixedCellCount: Integer;
    FirstGridCell: Integer;
    GridCellCount: Integer;
    GetExtent: TGetExtentsFunc;
  end;

  TGridDrawInfo = record
    Horz, Vert: TGridAxisDrawInfo;
  end;

  TGridState = (gsNormal, gsSelecting, gsColSizing, gsRowMoving, gsColMoving);
  TGridMovement = gsRowMoving..gsColMoving;

  { TInplaceEdit }
  { The inplace editor is not intended to be used outside the grid }

  TCustomGrid = class;

  TInplaceEdit = class(TCustomMaskEdit)
  private
    FGrid: TCustomGrid;
    FClickTime: Longint;
    procedure InternalMove(const Loc: TRect; Redraw: Boolean);
    procedure SetGrid(Value: TCustomGrid);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure BoundsChanged; virtual;
    procedure UpdateContents; virtual;
    procedure WndProc(var Message: TMessage); override;
    property  Grid: TCustomGrid read FGrid;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Deselect;
    procedure Hide;
    procedure Invalidate; reintroduce;
    procedure Move(const Loc: TRect);
    function PosEqual(const Rect: TRect): Boolean;
    procedure SetFocus; reintroduce;
    procedure UpdateLoc(const Loc: TRect);
    function Visible: Boolean;
  end;

  { TCustomGrid }

  TGridOption = (goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    goRangeSelect, goDrawFocusSelected, goRowSizing, goColSizing, goRowMoving,
    goColMoving, goEditing, goTabs, goRowSelect, goAlwaysShowEditor,
    goThumbTracking, goFixedColClick, goFixedRowClick, goFixedHotTrack);
  TGridOptions = set of TGridOption;
  TGridDrawState = set of (gdSelected, gdFocused, gdFixed, gdRowSelected,
    gdHotTrack, gdPressed);
  TGridScrollDirection = set of (sdLeft, sdRight, sdUp, sdDown);

  TGridCoord = record
    X: Longint;
    Y: Longint;
  end;

  THotTrackCellInfo = record
    Coord: TGridCoord;
    Pressed: Boolean;
    Button: TMouseButton;
  end;

  TGridRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Longint);
      1: (TopLeft, BottomRight: TGridCoord);
  end;

  TEditStyle =  (esSimple, esEllipsis, esPickList);

  TSelectCellEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    var CanSelect: Boolean) of object;
  TDrawCellEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    Rect: TRect; State: TGridDrawState) of object;
  TFixedCellClickEvent = procedure (Sender: TObject; ACol, ARow: Longint) of object;

  TGridDrawingStyle = (gdsClassic, gdsThemed, gdsGradient);

  TCustomGrid = class(TCustomControl)
  private
    FAnchor: TGridCoord;
    FBorderStyle: TBorderStyle;
    FCanEditModify: Boolean;
    FColCount: Longint;
    FCurrent: TGridCoord;
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
//    FDrawingStyle: TGridDrawingStyle;
    FGradientEndColor: TColor;
    FGradientStartColor: TColor;
    FGridLineWidth: Integer;
    FOptions: TGridOptions;
    FPanPoint: TPoint;
    FRowCount: Longint;
    FScrollBars: TScrollStyle;
    FTopLeft: TGridCoord;
    FSizingIndex: Longint;
    FSizingPos, FSizingOfs: Integer;
    FMoveIndex, FMovePos: Longint;
    FHitTest: TPoint;
    //FInplaceEdit: TInplaceEdit;
    FInplaceCol, FInplaceRow: Longint;
    FColOffset: Integer;
    FDefaultDrawing: Boolean;
    FEditorMode: Boolean;
    FColWidths: Pointer;
    FRowHeights: Pointer;
    FTabStops: Pointer;
    FOnFixedCellClick: TFixedCellClickEvent;
    FHighlightingColor: TColor;
    FLinesColor: TColor;
    FChangedValueColor: TColor;
    function CalcCoordFromPoint(X, Y: Integer;
      const DrawInfo: TGridDrawInfo): TGridCoord;
    procedure CalcDrawInfoXY(var DrawInfo: TGridDrawInfo;
      UseWidth, UseHeight: Integer);
    function CalcMaxTopLeft(const Coord: TGridCoord;
      const DrawInfo: TGridDrawInfo): TGridCoord;
    procedure CancelMode;
    procedure ChangeSize(NewColCount, NewRowCount: Longint);
    procedure ClampInView(const Coord: TGridCoord);
    procedure DrawMove;
    procedure GridRectToScreenRect(GridRect: TGridRect;
      var ScreenRect: TRect; IncludeLine: Boolean);
    procedure Initialize;
    procedure InvalidateRect(ARect: TGridRect);
    procedure ModifyScrollBar(ScrollBar, ScrollCode, Pos: Cardinal;
      UseRightToLeft: Boolean);
    procedure MoveAdjust(var CellPos: Longint; FromIndex, ToIndex: Longint);
    procedure MoveAnchor(const NewAnchor: TGridCoord);
    procedure MoveAndScroll(Mouse, CellHit: Integer; var DrawInfo: TGridDrawInfo;
      var Axis: TGridAxisDrawInfo; Scrollbar: Integer; const MousePt: TPoint);
    procedure MoveCurrent(ACol, ARow: Longint; MoveAnchor, Show: Boolean);
    procedure MoveTopLeft(ALeft, ATop: Longint);
    procedure ResizeCol(Index: Longint; OldSize, NewSize: Integer);
    procedure ResizeRow(Index: Longint; OldSize, NewSize: Integer);
    procedure SelectionMoved(const OldSel: TGridRect);
    procedure ScrollDataInfo(DX, DY: Integer; var DrawInfo: TGridDrawInfo);
    procedure TopLeftMoved(const OldTopLeft: TGridCoord);
    procedure UpdateScrollPos;
    procedure UpdateScrollRange;
    function GetColWidths(Index: Longint): Integer;
    function GetRowHeights(Index: Longint): Integer;
    function GetSelection: TGridRect;
    function GetTabStops(Index: Longint): Boolean;
    function GetVisibleColCount: Integer;
    function GetVisibleRowCount: Integer;
    function IsActiveControl: Boolean;
    function IsGradientEndColorStored: Boolean;
    procedure ReadColWidths(Reader: TReader);
    procedure ReadRowHeights(Reader: TReader);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCol(Value: Longint);
    procedure SetColCount(Value: Longint);
    procedure SetColWidths(Index: Longint; Value: Integer);
    procedure SetDefaultColWidth(Value: Integer);
    procedure SetDefaultRowHeight(Value: Integer);
//    procedure SetDrawingStyle(const Value: TGridDrawingStyle);
    procedure SetEditorMode(Value: Boolean);
    procedure SetGradientEndColor(Value: TColor);
    procedure SetGradientStartColor(Value: TColor);
    procedure SetGridLineWidth(Value: Integer);
    procedure SetLeftCol(Value: Longint);
    procedure SetOptions(Value: TGridOptions);
    procedure SetHighlightingColor(Value: TColor);
    procedure SetLinesColor(Value: TColor);
    procedure SetChangedValueColor(Value: TColor);
    procedure SetRow(Value: Longint);
    procedure SetRowCount(Value: Longint);
    procedure SetRowHeights(Index: Longint; Value: Integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetSelection(Value: TGridRect);
    procedure SetTabStops(Index: Longint; Value: Boolean);
    procedure SetTopRow(Value: Longint);
    procedure UpdateEdit;
    procedure UpdateText;
    procedure WriteColWidths(Writer: TWriter);
    procedure WriteRowHeights(Writer: TWriter);
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMCancelMode(var Msg: TWMCancelMode); message WM_CANCELMODE;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    FGridState: TGridState;
    FSaveCellExtents: Boolean;
    DesignOptionsBoost: TGridOptions;
    VirtualView: Boolean;
    FInternalColor: TColor;
    FInternalDrawingStyle: TGridDrawingStyle;
    FHotTrackCell: THotTrackCellInfo;
    procedure CalcDrawInfo(var DrawInfo: TGridDrawInfo);
    procedure CalcFixedInfo(var DrawInfo: TGridDrawInfo);
    procedure CalcSizingState(X, Y: Integer; var State: TGridState;
      var Index: Longint; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TGridDrawInfo); virtual;
    procedure ChangeGridOrientation(RightToLeftOrientation: Boolean);
    function CreateEditor: TInplaceEdit; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure AdjustSize(Index, Amount: Longint; Rows: Boolean); reintroduce; dynamic;
    function BoxRect(ALeft, ATop, ARight, ABottom: Longint): TRect;
    procedure DoExit; override;
    function CellRect(ACol, ARow: Longint): TRect;
    function CanEditAcceptKey(Key: Char): Boolean; dynamic;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; dynamic;
    function CanEditModify: Boolean; dynamic;
    function CanEditShow: Boolean; virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure FixedCellClick(ACol, ARow: Longint); dynamic;
    procedure FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
    function GetEditText(ACol, ARow: Longint): string; dynamic;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); dynamic;
    function GetEditLimit: Integer; dynamic;
    function GetEditMask(ACol, ARow: Longint): string; dynamic;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; dynamic;
    function GetGridWidth: Integer;
    function GetGridHeight: Integer;
    procedure HideEdit;
    procedure HideEditor;
    procedure ShowEditor;
    procedure ShowEditorChar(Ch: Char);
    procedure InvalidateEditor;
    procedure InvalidateGrid; inline;
    procedure MoveColumn(FromIndex, ToIndex: Longint);
    procedure ColumnMoved(FromIndex, ToIndex: Longint); dynamic;
    procedure MoveRow(FromIndex, ToIndex: Longint);
    procedure RowMoved(FromIndex, ToIndex: Longint); dynamic;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); virtual; abstract;
    procedure DrawCellBackground(const ARect: TRect; AColor: TColor;
      AState: TGridDrawState; ACol, ARow: Integer); virtual;
    procedure DrawCellHighlight(const ARect: TRect;
      AState: TGridDrawState; ACol, ARow: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure MoveColRow(ACol, ARow: Longint; MoveAnchor, Show: Boolean);
    function SelectCell(ACol, ARow: Longint): Boolean; virtual;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); dynamic;
    function Sizing(X, Y: Integer): Boolean;
    procedure ScrollData(DX, DY: Integer);
    procedure InvalidateCell(ACol, ARow: Longint);
    procedure InvalidateCol(ACol: Longint);
    procedure InvalidateRow(ARow: Longint);
    function IsTouchPropertyStored(AProperty: TTouchProperty): Boolean; override;
    procedure TopLeftChanged; dynamic;
    procedure TimedScroll(Direction: TGridScrollDirection); dynamic;
    procedure Paint; override;
    procedure ColWidthsChanged; dynamic;
    procedure RowHeightsChanged; dynamic;
    procedure DeleteColumn(ACol: Longint); virtual;
    procedure DeleteRow(ARow: Longint); virtual;
    procedure UpdateDesigner;
    function BeginColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function BeginRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function CheckRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function EndColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function EndRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Col: Longint read FCurrent.X write SetCol;
    property Color default clWindow;
    property ColCount: Longint read FColCount write SetColCount default 5;
    property ColWidths[Index: Longint]: Integer read GetColWidths write SetColWidths;
    property DefaultColWidth: Integer read FDefaultColWidth write SetDefaultColWidth default 64;
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing default True;
    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight default 24;
//    property DrawingStyle: TGridDrawingStyle read FDrawingStyle write SetDrawingStyle default gdsThemed;
    property EditorMode: Boolean read FEditorMode write SetEditorMode;
    property GradientEndColor: TColor read FGradientEndColor
      write SetGradientEndColor stored IsGradientEndColorStored;
    property GradientStartColor: TColor read FGradientStartColor
      write SetGradientStartColor default clWhite;
    property GridHeight: Integer read GetGridHeight;
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth default 1;
    property GridWidth: Integer read GetGridWidth;
    property HitTest: TPoint read FHitTest;
    //property InplaceEditor: TInplaceEdit read FInplaceEdit;
    property LeftCol: Longint read FTopLeft.X write SetLeftCol;
    property Options: TGridOptions read FOptions write SetOptions
      default [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
      goRangeSelect];
    property ParentColor default False;
    property Row: Longint read FCurrent.Y write SetRow;
    property RowCount: Longint read FRowCount write SetRowCount default 5;
    property RowHeights[Index: Longint]: Integer read GetRowHeights write SetRowHeights;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Selection: TGridRect read GetSelection write SetSelection;
    property TabStops[Index: Longint]: Boolean read GetTabStops write SetTabStops;
    property TopRow: Longint read FTopLeft.Y write SetTopRow;
    property VisibleColCount: Integer read GetVisibleColCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property OnFixedCellClick: TFixedCellClickEvent read FOnFixedCellClick write FOnFixedCellClick;
    property HighlightingColor: TColor read FHighlightingColor write SetHighlightingColor;
    property LinesColor: TColor read FLinesColor write SetLinesColor;
    property ChangedValueColor: TColor read FChangedValueColor write SetChangedValueColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MouseCoord(X, Y: Integer): TGridCoord;
  published
    property TabStop default True;
  end;

  { TCustomDrawGrid }

  TGetEditEvent = procedure (Sender: TObject; ACol, ARow: Longint; var Value: string) of object;
  TSetEditEvent = procedure (Sender: TObject; ACol, ARow: Longint; const Value: string) of object;
  TMovedEvent = procedure (Sender: TObject; FromIndex, ToIndex: Longint) of object;

  TCustomDrawGrid = class(TCustomGrid)
  private
    FOnColumnMoved: TMovedEvent;
    FOnDrawCell: TDrawCellEvent;
    FOnGetEditMask: TGetEditEvent;
    FOnGetEditText: TGetEditEvent;
    FOnRowMoved: TMovedEvent;
    FOnSelectCell: TSelectCellEvent;
    FOnSetEditText: TSetEditEvent;
    FOnTopLeftChanged: TNotifyEvent;
  protected
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure TopLeftChanged; override;
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnGetEditMask: TGetEditEvent read FOnGetEditMask write FOnGetEditMask;
    property OnGetEditText: TGetEditEvent read FOnGetEditText write FOnGetEditText;
    property OnRowMoved: TMovedEvent read FOnRowMoved write FOnRowMoved;
    property OnSelectCell: TSelectCellEvent read FOnSelectCell write FOnSelectCell;
    property OnSetEditText: TSetEditEvent read FOnSetEditText write FOnSetEditText;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
  public
    function CellRect(ACol, ARow: Longint): TRect;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    property Canvas;
    property Col;
    property ColWidths;
//    property DrawingStyle;
    property EditorMode;
    property GridHeight;
    property GridWidth;
    property LeftCol;
    property Selection;
    property Row;
    property RowHeights;
    property TabStops;
    property TopRow;
  end;

  { TDrawGrid }

  TDrawGrid = class(TCustomDrawGrid)
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property HighlightingColor;
    property LinesColor;
    property ChangedValueColor;
    property ColCount;
    property Constraints;
    property Ctl3D;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
//    property DrawingStyle;
    property Enabled;
    property RowCount;
    property Font;
    property GradientEndColor;
    property GradientStartColor;
    property GridLineWidth;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Touch;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
    property OnClick;
    property OnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFixedCellClick;
    property OnGesture;
    property OnGetEditMask;
    property OnGetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
  end;

  { TStringGrid }

  // ������� ������ ��� ��������� �������� �������� �������
  // ��� ��� ����������� ��������� � ������
  TDBGRegistersView = class(TDrawGrid)
  private
    FUpdating: Boolean;
    FNeedsUpdating: Boolean;
    FEditUpdate: Integer;
    FData: TCustomData;
    FRows: TCustomData;
    FCols: TCustomData;
    FValues: TValuesList;
    procedure DisableEditUpdate;
    procedure EnableEditUpdate;
    procedure Initialize;
    procedure Update(ACol, ARow: Integer); reintroduce;
    procedure SetUpdateState(Updating: Boolean);
    function EnsureDataRow(ARow: Integer): TCustomData;
    procedure OnValuesChange(Sender: TObject);
  protected
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Values: TValuesList read FValues write FValues;
  end;

  // �������� ���� ��� ����������� �����
  // �������, ����� �������� ���� �������� � �������
  TDBGStackView = class(TDrawGrid)
  private
    FUpdating: Boolean;
    FNeedsUpdating: Boolean;
    FEditUpdate: Integer;
    FData: TCustomData;
    FRows: TCustomData;
    FCols: TCustomData;

    FMemoryData: Pointer;
    FBytesPerRow: Integer;
    FStackHeadAddress: Int64;
    FMemoryDataSize: Int64;
    FHeadCellHighlightColor: TColor;
    FHeadCellHighlightFontColor: TColor;

    procedure DisableEditUpdate;
    procedure EnableEditUpdate;
    procedure Initialize;
    procedure Update(ACol, ARow: Integer); reintroduce;
    procedure SetUpdateState(Updating: Boolean);
    function EnsureDataRow(ARow: Integer): TCustomData;
    procedure OnValuesChange(Sender: TObject);

    procedure SetMemoryData(const aValue: pointer);
    procedure SetBytesPerRow(const aValue: integer);
    procedure SetStackHeadAddress(const aValue: Int64);
    procedure SetMemoryDataSize(const Value: Int64);
    procedure SetHeadCellHighlightColor(const Value: TColor);
    procedure SetHeadCellHighlightFontColor(const Value: TColor);
  protected
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
  public
    property MemoryData: Pointer read FMemoryData write SetMemoryData;
    property StackHeadAddress : Int64 read FStackHeadAddress write SetStackHeadAddress;
    property MemoryDataSize: Int64 read FMemoryDataSize write SetMemoryDataSize;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BytesPerRow: Integer read FBytesPerRow write SetBytesPerRow;
    property HeadCellHighlightColor: TColor read FHeadCellHighlightColor write SetHeadCellHighlightColor;
    property HeadCellHighlightFontColor: TColor read FHeadCellHighlightFontColor write SetHeadCellHighlightFontColor;
  end;

  // ��������������� ����� - ���������, �������� �� ���� ������������������� ������� ����
  PDisasmItem = ^TDisasmItem;
  TDisasmItem = packed record
    Address: Cardinal; // �����
    Segment: Cardinal; // ����� �������� ����
    Size: Byte; // ���-�� ���� � �������
    Opcode: string[16]; // ����� hex �������
    Disasm: string[32]; // ������ ������
  end;

  TParseProc = procedure(aDataPtr: pointer; var aItemInfo: TDisasmItem) of object;

  TMiniDisasm = class
  private
    FMemoryData: pointer;
    FMemoryDataSize: Int64;
    FList: TList;
    FSegmentOffset: int64;
    FSegmentOffsetSize: int64;
    FOnParseCode: TParseProc;

    procedure AddItem(const aItem: TDisasmItem);
    procedure SetMemoryData(const Value: pointer);
    procedure SetMemoryDataSize(const Value: Int64);
    procedure SetSegmentOffset(const Value: int64);
    procedure SetSegmentOffsetSize(const Value: int64);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property MemoryData: pointer read FMemoryData write SetMemoryData;
    property MemoryDataSize: Int64 read FMemoryDataSize write SetMemoryDataSize;
    property SegmentOffset: int64 read FSegmentOffset write SetSegmentOffset;
    property SegmentOffsetSize: int64 read FSegmentOffsetSize write SetSegmentOffsetSize;

    procedure Parse;
    procedure Clear;
    function Count: integer;
    function GetItem(const index: integer): TDisasmItem;
    function IndexOfAddress(const addr: int64): int64;
  published
    property OnParseCode: TParseProc read FOnParseCode write FOnParseCode;
  end;

  TDBGCPUView = class(TDrawGrid)
  private
    FUpdating: Boolean;
    FNeedsUpdating: Boolean;
    FEditUpdate: Integer;
    FData: TCustomData;
    FRows: TCustomData;
    FCols: TCustomData;

    FMemoryData: Pointer;
    FMemoryDataSize: Int64;

    FCodeSegmentValue: int64;
    FCodeSegmentSize: int64;
    FCurrentLine: integer;

    FDisasm: TMiniDisasm;
    FOnParseCode: TParseProc;

    FEIPCellHighlightColor: TColor;
    FEIPCellHighlightFontColor: TColor;

    procedure DisableEditUpdate;
    procedure EnableEditUpdate;
    procedure Initialize;
    procedure Update(ACol, ARow: Integer); reintroduce;
    procedure SetUpdateState(Updating: Boolean);
    function EnsureDataRow(ARow: Integer): TCustomData;
    procedure OnValuesChange(Sender: TObject);

    procedure SetMemoryData(const aValue: pointer);
    procedure SetMemoryDataSize(const Value: Int64);
    procedure SetCodeSegmentValue(const Value: Int64);
    procedure SetCodeSegmentSize(const Value: Int64);
    procedure SetCurrentLine(const Value: integer);

    procedure DisasmOnParse(aDataPtr: pointer; var aItemInfo: TDisasmItem);
    procedure SetEIPCellHighlightColor(const Value: TColor);
    procedure SetEIPCellHighlightFontColor(const Value: TColor);
  protected
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
  public
    procedure UpdateDisasm; // ������������� �������

    property MemoryData: Pointer read FMemoryData write SetMemoryData;
    property MemoryDataSize: Int64 read FMemoryDataSize write SetMemoryDataSize;
    property CodeSegmentValue: int64 read FCodeSegmentValue write SetCodeSegmentValue;
    property CodeSegmentSize: int64 read FCodeSegmentSize write SetCodeSegmentSize;
    property CurrentLine: integer read FCurrentLine write SetCurrentLine;

    property HeadCellHighlightColor: TColor read FEIPCellHighlightColor write SetEIPCellHighlightColor;
    property HeadCellHighlightFontColor: TColor read FEIPCellHighlightFontColor write SetEIPCellHighlightFontColor;

    property OnParseCode: TParseProc read FOnParseCode write FOnParseCode;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  // �������� ���� ��� �����

implementation

uses
  Math, RTLConsts, Consts, GraphUtil;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxCustomExtents] of Integer;

procedure InvalidOp(const id: string);
begin
  raise Exception.Create(id);
end;

function GridRect(Coord1, Coord2: TGridCoord): TGridRect;
begin
  with Result do
  begin
    Left := Coord2.X;
    if Coord1.X < Coord2.X then Left := Coord1.X;
    Right := Coord1.X;
    if Coord1.X < Coord2.X then Right := Coord2.X;
    Top := Coord2.Y;
    if Coord1.Y < Coord2.Y then Top := Coord1.Y;
    Bottom := Coord1.Y;
    if Coord1.Y < Coord2.Y then Bottom := Coord2.Y;
  end;
end;

function PointInGridRect(Col, Row: Longint; const Rect: TGridRect): Boolean;
begin
  Result := (Col >= Rect.Left) and (Col <= Rect.Right) and (Row >= Rect.Top)
    and (Row <= Rect.Bottom);
end;

type
  TXorRects = array[0..3] of TRect;

procedure XorRects(const R1, R2: TRect; var XorRects: TXorRects);
var
  Intersect, Union: TRect;

  function PtInRect(X, Y: Integer; const Rect: TRect): Boolean;
  begin
    with Rect do Result := (X >= Left) and (X <= Right) and (Y >= Top) and
      (Y <= Bottom);
  end;


  function Includes(const P1: TPoint; var P2: TPoint): Boolean;
  begin
    with P1 do
    begin
      Result := PtInRect(X, Y, R1) or PtInRect(X, Y, R2);
      if Result then P2 := P1;
    end;
  end;

  function Build(var R: TRect; const P1, P2, P3: TPoint): Boolean;
  begin
    Build := True;
    with R do
      if Includes(P1, TopLeft) then
      begin
        if not Includes(P3, BottomRight) then BottomRight := P2;
      end
      else if Includes(P2, TopLeft) then BottomRight := P3
      else Build := False;
  end;

begin
  FillChar(XorRects, SizeOf(XorRects), 0);
  if not IntersectRect(Intersect, R1, R2) then
  begin
    { Don't intersect so its simple }
    XorRects[0] := R1;
    XorRects[1] := R2;
  end
  else
  begin
    UnionRect(Union, R1, R2);
    if Build(XorRects[0],
      Point(Union.Left, Union.Top),
      Point(Union.Left, Intersect.Top),
      Point(Union.Left, Intersect.Bottom)) then
      XorRects[0].Right := Intersect.Left;
    if Build(XorRects[1],
      Point(Intersect.Left, Union.Top),
      Point(Intersect.Right, Union.Top),
      Point(Union.Right, Union.Top)) then
      XorRects[1].Bottom := Intersect.Top;
    if Build(XorRects[2],
      Point(Union.Right, Intersect.Top),
      Point(Union.Right, Intersect.Bottom),
      Point(Union.Right, Union.Bottom)) then
      XorRects[2].Left := Intersect.Right;
    if Build(XorRects[3],
      Point(Union.Left, Union.Bottom),
      Point(Intersect.Left, Union.Bottom),
      Point(Intersect.Right, Union.Bottom)) then
      XorRects[3].Top := Intersect.Bottom;
  end;
end;

procedure ModifyExtents(var Extents: Pointer; Index, Amount: Longint;
  Default: Integer);
var
  LongSize, OldSize: LongInt;
  NewSize: Integer;
  I: Integer;
begin
  if Amount <> 0 then
  begin
    if not Assigned(Extents) then OldSize := 0
    else OldSize := PIntArray(Extents)^[0];
    if (Index < 0) or (OldSize < Index) then InvalidOp(SIndexOutOfRange);
    LongSize := OldSize + Amount;
    if LongSize < 0 then InvalidOp(STooManyDeleted)
    else if LongSize >= MaxListSize - 1 then InvalidOp(SGridTooLarge);
    NewSize := Cardinal(LongSize);
    if NewSize > 0 then Inc(NewSize);
    ReallocMem(Extents, NewSize * SizeOf(Integer));
    if Assigned(Extents) then
    begin
      I := Index + 1;
      while I < NewSize do
      begin
        PIntArray(Extents)^[I] := Default;
        Inc(I);
      end;
      PIntArray(Extents)^[0] := NewSize-1;
    end;
  end;
end;

procedure UpdateExtents(var Extents: Pointer; NewSize: Longint;
  Default: Integer);
var
  OldSize: Integer;
begin
  OldSize := 0;
  if Assigned(Extents) then OldSize := PIntArray(Extents)^[0];
  ModifyExtents(Extents, OldSize, NewSize - OldSize, Default);
end;


procedure MoveExtent(var Extents: Pointer; FromIndex, ToIndex: Longint);
var
  Extent: Integer;
begin
  if Assigned(Extents) then
  begin
    Extent := PIntArray(Extents)^[FromIndex];
    if FromIndex < ToIndex then
      Move(PIntArray(Extents)^[FromIndex + 1], PIntArray(Extents)^[FromIndex],
        (ToIndex - FromIndex) * SizeOf(Integer))
    else if FromIndex > ToIndex then
      Move(PIntArray(Extents)^[ToIndex], PIntArray(Extents)^[ToIndex + 1],
        (FromIndex - ToIndex) * SizeOf(Integer));
    PIntArray(Extents)^[ToIndex] := Extent;
  end;
end;

function CompareExtents(E1, E2: Pointer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if E1 <> nil then
  begin
    if E2 <> nil then
    begin
      for I := 0 to PIntArray(E1)^[0] do
        if PIntArray(E1)^[I] <> PIntArray(E2)^[I] then Exit;
      Result := True;
    end
  end
  else Result := E2 = nil;
end;

{ Private. LongMulDiv multiplys the first two arguments and then
  divides by the third.  This is used so that real number
  (floating point) arithmetic is not necessary.  This routine saves
  the possible 64-bit value in a temp before doing the divide.  Does
  not do error checking like divide by zero.  Also assumes that the
  result is in the 32-bit range (Actually 31-bit, since this algorithm
  is for unsigned). }

function LongMulDiv(Mult1, Mult2, Div1: Longint): Longint; stdcall;
  external 'kernel32.dll' name 'MulDiv';

procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

constructor TInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  DoubleBuffered := False;
end;

procedure TInplaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE;
end;

procedure TInplaceEdit.SetGrid(Value: TCustomGrid);
begin
  FGrid := Value;
end;

procedure TInplaceEdit.CMShowingChanged(var Message: TMessage);
begin
  { Ignore showing using the Visible property }
end;

procedure TInplaceEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if goTabs in Grid.Options then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

[UIPermission(SecurityAction.LinkDemand, Clipboard=UIPermissionClipboard.AllClipboard)]
procedure TInplaceEdit.WMPaste(var Message: TMessage);
begin
  if not EditCanModify then Exit;
  inherited
end;

procedure TInplaceEdit.WMClear(var Message: TMessage);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TInplaceEdit.WMCut(var Message: TMessage);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TInplaceEdit.DblClick;
begin
  Grid.DblClick;
end;

function TInplaceEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := Grid.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TInplaceEdit.EditCanModify: Boolean;
begin
  Result := Grid.CanEditModify;
end;

procedure TInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    Grid.KeyDown(Key, Shift);
    Key := 0;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    GridKeyDown := Grid.OnKeyDown;
    if Assigned(GridKeyDown) then GridKeyDown(Grid, Key, Shift);
  end;

  function ForwardMovement: Boolean;
  begin
    Result := goAlwaysShowEditor in Grid.Options;
  end;

  function Ctrl: Boolean;
  begin
    Result := ssCtrl in Shift;
  end;

  function Selection: TSelection;
  begin
    SendMessage(Handle, EM_GETSEL, Longint(@Result.StartPos), Longint(@Result.EndPos));
  end;

  function CaretPos: Integer;
  var
    P: TPoint;
  begin
    Windows.GetCaretPos(P);
    Result := SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLong(P.X, P.Y));
  end;

  function RightSide: Boolean;
  begin
    with Selection do
      Result := (CaretPos = GetTextLen) and
        ((StartPos = 0) or (EndPos = StartPos)) and (EndPos = GetTextLen);
   end;

  function LeftSide: Boolean;
  begin
    with Selection do
      Result := (CaretPos = 0) and (StartPos = 0) and
        ((EndPos = 0) or (EndPos = GetTextLen));
  end;

begin
  case Key of
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_ESCAPE: SendToParent;
    VK_INSERT:
      if Shift = [] then SendToParent
      else if (Shift = [ssShift]) and not Grid.CanEditModify then Key := 0;
    VK_LEFT: if ForwardMovement and (Ctrl or LeftSide) then SendToParent;
    VK_RIGHT: if ForwardMovement and (Ctrl or RightSide) then SendToParent;
    VK_HOME: if ForwardMovement and (Ctrl or LeftSide) then SendToParent;
    VK_END: if ForwardMovement and (Ctrl or RightSide) then SendToParent;
    VK_F2:
      begin
        ParentEvent;
        if Key = VK_F2 then
        begin
          Deselect;
          Exit;
        end;
      end;
    VK_TAB: if not (ssAlt in Shift) then SendToParent;
    VK_DELETE:
      if Ctrl then
        SendToParent
      else
        if not Grid.CanEditModify then Key := 0;
  end;
  if Key <> 0 then
  begin
    ParentEvent;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TInplaceEdit.KeyPress(var Key: Char);
var
  Selection: TSelection;
begin
  Grid.KeyPress(Key);
  if (Key >= #32) and not Grid.CanEditAcceptKey(Key) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
  case Key of
    #9, #27: Key := #0;
    #13:
      begin
        SendMessage(Handle, EM_GETSEL, Longint(@Selection.StartPos), Longint(@Selection.EndPos));
        if (Selection.StartPos = 0) and (Selection.EndPos = GetTextLen) then
          Deselect else
          SelectAll;
        Key := #0;
      end;
    ^H, ^V, ^X, #32..High(Char):
      if not Grid.CanEditModify then Key := #0;
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Grid.KeyUp(Key, Shift);
end;

procedure TInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or GetParentForm(Self).SetFocusedControl(Grid) then
          Dispatch(Message);
        Exit;
      end;
    WM_LBUTTONDOWN:
      begin
        if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
          Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TInplaceEdit.Deselect;
begin
  SendMessage(Handle, EM_SETSEL, $7FFFFFFF, Longint($FFFFFFFF));
end;

procedure TInplaceEdit.Invalidate;
var
  Cur: TRect;
begin
  ValidateRect(Handle, nil);
  InvalidateRect(Handle, nil, True);
  Windows.GetClientRect(Handle, Cur);
  MapWindowPoints(Handle, Grid.Handle, Cur, 2);
  ValidateRect(Grid.Handle, Cur);
  InvalidateRect(Grid.Handle, Cur, False);
end;

procedure TInplaceEdit.Hide;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
  begin
    Invalidate;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER or
      SWP_NOREDRAW);
    if Focused then Windows.SetFocus(Grid.Handle);
  end;
end;

function TInplaceEdit.PosEqual(const Rect: TRect): Boolean;
var
  Cur: TRect;
begin
  GetWindowRect(Handle, Cur);
  MapWindowPoints(HWND_DESKTOP, Grid.Handle, Cur, 2);
  Result := EqualRect(Rect, Cur);
end;

procedure TInplaceEdit.InternalMove(const Loc: TRect; Redraw: Boolean);
begin
  if IsRectEmpty(Loc) then Hide
  else
  begin
    CreateHandle;
    Redraw := Redraw or not IsWindowVisible(Handle);
    Invalidate;
    with Loc do
      SetWindowPos(Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top,
        SWP_SHOWWINDOW or SWP_NOREDRAW);
    BoundsChanged;
    if Redraw then Invalidate;
    if Grid.Focused then
      Windows.SetFocus(Handle);
  end;
end;

procedure TInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  R := Rect(2, 2, Width - 2, Height);
  SendStructMessage(Handle, EM_SETRECTNP, 0, R);
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TInplaceEdit.UpdateLoc(const Loc: TRect);
begin
  InternalMove(Loc, False);
end;

function TInplaceEdit.Visible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TInplaceEdit.Move(const Loc: TRect);
begin
  InternalMove(Loc, True);
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TInplaceEdit.SetFocus;
begin
  if IsWindowVisible(Handle) then
    Windows.SetFocus(Handle);
end;

procedure TInplaceEdit.UpdateContents;
begin
  Text := '';
  EditMask := Grid.GetEditMask(Grid.Col, Grid.Row);
  Text := Grid.GetEditText(Grid.Col, Grid.Row);
  MaxLength := Grid.GetEditLimit;
end;

{ TCustomGrid }

const
  GradientEndColorBase = $F0F0F0;

constructor TCustomGrid.Create(AOwner: TComponent);
const
  GridStyle = [csCaptureMouse, csOpaque, csDoubleClicks,
                csNeedsBorderPaint, csPannable, csGestures];
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := GridStyle
  else
    ControlStyle := GridStyle + [csFramed];

  FCanEditModify := True;
  FColCount := 5;
  FRowCount := 5;
  FGridLineWidth := 1;
  FOptions := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    goRangeSelect];
  DesignOptionsBoost := [goColSizing, goRowSizing];
  FScrollBars := ssBoth;
  FBorderStyle := bsSingle;
  FDefaultColWidth := 64;
  FDefaultRowHeight := 24;
  FDefaultDrawing := True;
  FChangedValueColor := clRed;
//  FDrawingStyle := gdsThemed;
  FGradientEndColor := GetShadowColor(GradientEndColorBase, -25);
  FGradientStartColor := clWhite;
  FSaveCellExtents := True;
  FEditorMode := False;
  Color := clWindow;
  FHighlightingColor := clRed;
  ParentColor := False;
  TabStop := True;
  SetBounds(Left, Top, FColCount * FDefaultColWidth,
    FRowCount * FDefaultRowHeight);
  FHotTrackCell.Coord.X := -1;
  FHotTrackCell.Coord.Y := -1;
  FHotTrackCell.Pressed := False;
  Touch.InteractiveGestures := [igPan, igPressAndTap];
  Touch.InteractiveGestureOptions := [igoPanInertia,
    igoPanSingleFingerHorizontal, igoPanSingleFingerVertical,
    igoPanGutter, igoParentPassthrough];
  Initialize;
end;

destructor TCustomGrid.Destroy;
begin
//  FInplaceEdit.Free;
//  FInplaceEdit := nil;
  inherited Destroy;
  FreeMem(FColWidths);
  FreeMem(FRowHeights);
  FreeMem(FTabStops);
end;

procedure TCustomGrid.AdjustSize(Index, Amount: Longint; Rows: Boolean);
var
  NewCur: TGridCoord;
  OldRows, OldCols: Longint;
  MovementX, MovementY: Longint;
  MoveRect: TGridRect;
  ScrollArea: TRect;
  AbsAmount: Longint;

  function DoSizeAdjust(var Count: Longint; var Extents: Pointer;
    DefaultExtent: Integer; var Current: Longint): Longint;
  var
    I: Integer;
    NewCount: Longint;
  begin
    NewCount := Count + Amount;
    if NewCount < Index then InvalidOp(STooManyDeleted);
    if (Amount < 0) and Assigned(Extents) then
    begin
      Result := 0;
      for I := Index to Index - Amount - 1 do
        Inc(Result, PIntArray(Extents)^[I]);
    end
    else
      Result := Amount * DefaultExtent;
    if Extents <> nil then
      ModifyExtents(Extents, Index, Amount, DefaultExtent);
    Count := NewCount;
    if Current >= Index then
      if (Amount < 0) and (Current < Index - Amount) then Current := Index
      else Inc(Current, Amount);
  end;

begin
  if Amount = 0 then Exit;
  NewCur := FCurrent;
  OldCols := ColCount;
  OldRows := RowCount;
  MoveRect.Left := 0;
  MoveRect.Right := ColCount - 1;
  MoveRect.Top := 0;
  MoveRect.Bottom := RowCount - 1;
  MovementX := 0;
  MovementY := 0;
  AbsAmount := Amount;
  if AbsAmount < 0 then AbsAmount := -AbsAmount;
  if Rows then
  begin
    MovementY := DoSizeAdjust(FRowCount, FRowHeights, DefaultRowHeight, NewCur.Y);
    MoveRect.Top := Index;
    if Index + AbsAmount <= TopRow then MoveRect.Bottom := TopRow - 1;
  end
  else
  begin
    MovementX := DoSizeAdjust(FColCount, FColWidths, DefaultColWidth, NewCur.X);
    MoveRect.Left := Index;
    if Index + AbsAmount <= LeftCol then MoveRect.Right := LeftCol - 1;
  end;
  GridRectToScreenRect(MoveRect, ScrollArea, True);
  if not IsRectEmpty(ScrollArea) then
  begin
    ScrollWindow(Handle, MovementX, MovementY,
      @ScrollArea, @ScrollArea);
    UpdateWindow(Handle);
  end;
  SizeChanged(OldCols, OldRows);
  if (NewCur.X <> FCurrent.X) or (NewCur.Y <> FCurrent.Y) then
    MoveCurrent(NewCur.X, NewCur.Y, True, True);
end;

function TCustomGrid.BoxRect(ALeft, ATop, ARight, ABottom: Longint): TRect;
var
  GridRect: TGridRect;
begin
  GridRect.Left := ALeft;
  GridRect.Right := ARight;
  GridRect.Top := ATop;
  GridRect.Bottom := ABottom;
  GridRectToScreenRect(GridRect, Result, False);
end;

procedure TCustomGrid.DoExit;
begin
  inherited DoExit;
  if not (goAlwaysShowEditor in Options) then HideEditor;
end;

function TCustomGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := BoxRect(ACol, ARow, ACol, ARow);
end;

function TCustomGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  Result := True;
end;

function TCustomGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
end;

function TCustomGrid.CanEditModify: Boolean;
begin
  Result := FCanEditModify;
end;

function TCustomGrid.CanEditShow: Boolean;
begin
  Result := ([goRowSelect, goEditing] * Options = [goEditing]) and
    FEditorMode and not (csDesigning in ComponentState) and HandleAllocated and
    ((goAlwaysShowEditor in Options) or IsActiveControl);
end;

function TCustomGrid.IsActiveControl: Boolean;
var
  H: Hwnd;
  ParentForm: TCustomForm;
begin
  Result := False;
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
    Result := (ParentForm.ActiveControl = Self) and
      ((ParentForm = Screen.ActiveForm) or (ParentForm is TCustomActiveForm) or (ParentForm is TCustomDockForm))
  else
  begin
    H := GetFocus;
    while IsWindow(H) and not Result do
    begin
      if H = WindowHandle then
        Result := True
      else
        H := GetParent(H);
    end;
  end;
end;

function TCustomGrid.IsGradientEndColorStored: Boolean;
begin
  Result := FGradientEndColor <> GetShadowColor(GradientEndColorBase, -25);
end;

function TCustomGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
end;

function TCustomGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := '';
end;

procedure TCustomGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
end;

function TCustomGrid.GetEditLimit: Integer;
begin
  Result := 0;
end;

function TCustomGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esSimple;
end;

procedure TCustomGrid.HideEditor;
begin
  FEditorMode := False;
  HideEdit;
end;

procedure TCustomGrid.ShowEditor;
begin
  FEditorMode := True;
  UpdateEdit;
end;

procedure TCustomGrid.ShowEditorChar(Ch: Char);
begin
  ShowEditor;
//  if FInplaceEdit <> nil then
//    PostMessage(FInplaceEdit.Handle, WM_CHAR, Ord(Ch), 0);
end;

procedure TCustomGrid.InvalidateEditor;
begin
  FInplaceCol := -1;
  FInplaceRow := -1;
  UpdateEdit;
end;

procedure TCustomGrid.ReadColWidths(Reader: TReader);
var
  I: Integer;
begin
  with Reader do
  begin
    ReadListBegin;
    for I := 0 to ColCount - 1 do ColWidths[I] := ReadInteger;
    ReadListEnd;
  end;
end;

procedure TCustomGrid.ReadRowHeights(Reader: TReader);
var
  I: Integer;
begin
  with Reader do
  begin
    ReadListBegin;
    for I := 0 to RowCount - 1 do RowHeights[I] := ReadInteger;
    ReadListEnd;
  end;
end;

procedure TCustomGrid.WriteColWidths(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for I := 0 to ColCount - 1 do WriteInteger(ColWidths[I]);
    WriteListEnd;
  end;
end;

procedure TCustomGrid.WriteRowHeights(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for I := 0 to RowCount - 1 do WriteInteger(RowHeights[I]);
    WriteListEnd;
  end;
end;

procedure TCustomGrid.DefineProperties(Filer: TFiler);

  function DoColWidths: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not CompareExtents(TCustomGrid(Filer.Ancestor).FColWidths, FColWidths)
    else
      Result := FColWidths <> nil;
  end;

  function DoRowHeights: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not CompareExtents(TCustomGrid(Filer.Ancestor).FRowHeights, FRowHeights)
    else
      Result := FRowHeights <> nil;
  end;


begin
  inherited DefineProperties(Filer);
  if FSaveCellExtents then
    with Filer do
    begin
      DefineProperty('ColWidths', ReadColWidths, WriteColWidths, DoColWidths);
      DefineProperty('RowHeights', ReadRowHeights, WriteRowHeights, DoRowHeights);
    end;
end;

procedure TCustomGrid.MoveColumn(FromIndex, ToIndex: Longint);
var
  Rect: TGridRect;
begin
  if FromIndex = ToIndex then Exit;
  if Assigned(FColWidths) then
  begin
    MoveExtent(FColWidths, FromIndex + 1, ToIndex + 1);
    MoveExtent(FTabStops, FromIndex + 1, ToIndex + 1);
  end;
  MoveAdjust(FCurrent.X, FromIndex, ToIndex);
  MoveAdjust(FAnchor.X, FromIndex, ToIndex);
  MoveAdjust(FInplaceCol, FromIndex, ToIndex);
  Rect.Top := 0;
  Rect.Bottom := VisibleRowCount;
  if FromIndex < ToIndex then
  begin
    Rect.Left := FromIndex;
    Rect.Right := ToIndex;
  end
  else
  begin
    Rect.Left := ToIndex;
    Rect.Right := FromIndex;
  end;
  InvalidateRect(Rect);
  ColumnMoved(FromIndex, ToIndex);
  if Assigned(FColWidths) then
    ColWidthsChanged;
  UpdateEdit;
end;

procedure TCustomGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
end;

procedure TCustomGrid.MoveRow(FromIndex, ToIndex: Longint);
begin
  if Assigned(FRowHeights) then
    MoveExtent(FRowHeights, FromIndex + 1, ToIndex + 1);
  MoveAdjust(FCurrent.Y, FromIndex, ToIndex);
  MoveAdjust(FAnchor.Y, FromIndex, ToIndex);
  MoveAdjust(FInplaceRow, FromIndex, ToIndex);
  RowMoved(FromIndex, ToIndex);
  if Assigned(FRowHeights) then
    RowHeightsChanged;
  UpdateEdit;
end;

procedure TCustomGrid.RowMoved(FromIndex, ToIndex: Longint);
begin
end;

function TCustomGrid.MouseCoord(X, Y: Integer): TGridCoord;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := CalcCoordFromPoint(X, Y, DrawInfo);
  if Result.X < 0 then Result.Y := -1
  else if Result.Y < 0 then Result.X := -1;
end;

procedure TCustomGrid.MoveColRow(ACol, ARow: Longint; MoveAnchor,
  Show: Boolean);
begin
  MoveCurrent(ACol, ARow, MoveAnchor, Show);
end;

function TCustomGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
end;

procedure TCustomGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
end;

function TCustomGrid.Sizing(X, Y: Integer): Boolean;
var
  DrawInfo: TGridDrawInfo;
  State: TGridState;
  Index: Longint;
  Pos, Ofs: Integer;
begin
  State := FGridState;
  if State = gsNormal then
  begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end;
  Result := State <> gsNormal;
end;

procedure TCustomGrid.TopLeftChanged;
begin
//  if FEditorMode and (FInplaceEdit <> nil) then
//    FInplaceEdit.UpdateLoc(CellRect(Col, Row));
end;

procedure FillDWord(var Dest; Count, Value: Integer); register;
asm
  XCHG  EDX, ECX
  PUSH  EDI
  MOV   EDI, EAX
  MOV   EAX, EDX
  REP   STOSD
  POP   EDI
end;

{ StackAlloc allocates a 'small' block of memory from the stack by
  decrementing SP.  This provides the allocation speed of a local variable,
  but the runtime size flexibility of heap allocated memory.  }

function StackAlloc(Size: Integer): Pointer; register;
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          { make sure we touch guard page, to grow stack }
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;

{ StackFree pops the memory allocated by StackAlloc off the stack.
- Calling StackFree is optional - SP will be restored when the calling routine
  exits, but it's a good idea to free the stack allocated memory ASAP anyway.
- StackFree must be called in the same stack context as StackAlloc - not in
  a subroutine or finally block.
- Multiple StackFree calls must occur in reverse order of their corresponding
  StackAlloc calls.
- Built-in sanity checks guarantee that an improper call to StackFree will not
  corrupt the stack. Worst case is that the stack block is not released until
  the calling routine exits. }

procedure StackFree(P: Pointer); register;
asm
  POP   ECX                     { return address }
  MOV   EDX, DWORD PTR [ESP]
  SUB   EAX, 8
  CMP   EDX, ESP                { sanity check #1 (SP = [SP]) }
  JNE   @@1
  CMP   EDX, EAX                { sanity check #2 (P = this stack block) }
  JNE   @@1
  MOV   ESP, DWORD PTR [ESP+4]  { restore previous SP  }
@@1:
  PUSH  ECX                     { return to caller }
end;

procedure TCustomGrid.Paint;
var
  LColorRef: TColorRef;
  LineColor: TColor;
  DrawInfo: TGridDrawInfo;
  Sel: TGridRect;
  UpdateRect: TRect;
  AFocRect, FocRect: TRect;
  PointsList: PIntArray;
  StrokeList: PIntArray;
  MaxStroke: Integer;
  FrameFlags1, FrameFlags2: DWORD;

  procedure DrawLines(DoHorz, DoVert: Boolean; Col, Row: Longint;
    const CellBounds: array of Integer; OnColor, OffColor: TColor);

  { Cellbounds is 4 integers: StartX, StartY, StopX, StopY
    Horizontal lines:  MajorIndex = 0
    Vertical lines:    MajorIndex = 1 }

  const
    FlatPenStyle = PS_Geometric or PS_Solid or PS_EndCap_Flat or PS_Join_Miter;

    procedure DrawAxisLines(const AxisInfo: TGridAxisDrawInfo;
      Cell, MajorIndex: Integer; UseOnColor: Boolean);
    var
      Line: Integer;
      LogBrush: TLOGBRUSH;
      Index: Integer;
      Points: PIntArray;
      StopMajor, StartMinor, StopMinor, StopIndex: Integer;
      LineIncr: Integer;
    begin
      with Canvas, AxisInfo do
      begin
        if EffectiveLineWidth <> 0 then
        begin
          Pen.Width := GridLineWidth;
          if UseOnColor then
            Pen.Color := OnColor
          else
            Pen.Color := OffColor;
          if Pen.Width > 1 then
          begin
            LogBrush.lbStyle := BS_Solid;
            LogBrush.lbColor := Pen.Color;
            LogBrush.lbHatch := 0;
            Pen.Handle := ExtCreatePen(FlatPenStyle, Pen.Width, LogBrush, 0, nil);
          end;
          Points := PointsList;
          Line := CellBounds[MajorIndex] + (EffectiveLineWidth shr 1) +
            AxisInfo.GetExtent(Cell);
          //!!! ??? Line needs to be incremented for RightToLeftAlignment ???
          if UseRightToLeftAlignment and (MajorIndex = 0) then Inc(Line);
          StartMinor := CellBounds[MajorIndex xor 1];
          StopMinor := CellBounds[2 + (MajorIndex xor 1)];
          StopMajor := CellBounds[2 + MajorIndex] + EffectiveLineWidth;
          StopIndex := MaxStroke * 4;
          Index := 0;
          repeat
            Points^[Index + MajorIndex] := Line;         { MoveTo }
            Points^[Index + (MajorIndex xor 1)] := StartMinor;
            Inc(Index, 2);
            Points^[Index + MajorIndex] := Line;         { LineTo }
            Points^[Index + (MajorIndex xor 1)] := StopMinor;
            Inc(Index, 2);

            // Skip hidden columns/rows.  We don't have stroke slots for them
            // A column/row with an extent of -EffectiveLineWidth is hidden
            repeat
              Inc(Cell);
              LineIncr := AxisInfo.GetExtent(Cell) + EffectiveLineWidth;
            until (LineIncr > 0) or (Cell > LastFullVisibleCell);
            Inc(Line, LineIncr);
          until (Line > StopMajor) or (Cell > LastFullVisibleCell) or (Index > StopIndex);
           { 2 integers per point, 2 points per line -> Index div 4 }
          PolyPolyLine(Canvas.Handle, Points^, StrokeList^, Index shr 2);
        end;
      end;
    end;

  begin
    if (CellBounds[0] = CellBounds[2]) or (CellBounds[1] = CellBounds[3]) then
      Exit;
    if not DoHorz then
    begin
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
    end
    else
    begin
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
    end;
  end;

  procedure DrawCells(ACol, ARow: Longint; StartX, StartY, StopX, StopY: Integer;
    AColor: TColor; IncludeDrawState: TGridDrawState);
  var
    CurCol, CurRow: Longint;
    AWhere, Where, TempRect: TRect;
    DrawState: TGridDrawState;
    Focused: Boolean;
  begin
    CurRow := ARow;
    Where.Top := StartY;
    while (Where.Top < StopY) and (CurRow < RowCount) do
    begin
      CurCol := ACol;
      Where.Left := StartX;
      Where.Bottom := Where.Top + RowHeights[CurRow];
      while (Where.Left < StopX) and (CurCol < ColCount) do
      begin
        Where.Right := Where.Left + ColWidths[CurCol];
        if (Where.Right > Where.Left) and RectVisible(Canvas.Handle, Where) then
        begin
          DrawState := IncludeDrawState;
          if (CurCol = FHotTrackCell.Coord.X) and (CurRow = FHotTrackCell.Coord.Y) then
          begin
            if (goFixedHotTrack in Options) then
              Include(DrawState, gdHotTrack);
            if FHotTrackCell.Pressed then
              Include(DrawState, gdPressed);
          end;
          Focused := IsActiveControl;
          if Focused and (CurRow = Row) and (CurCol = Col)  then
          begin
            SetCaretPos(Where.Left, Where.Top);
            Include(DrawState, gdFocused);
          end;
          if PointInGridRect(CurCol, CurRow, Sel) then
            Include(DrawState, gdSelected);
          if not (gdFocused in DrawState) or not (goEditing in Options) or
            not FEditorMode or (csDesigning in ComponentState) then
          begin
            if DefaultDrawing or (csDesigning in ComponentState) then
            begin
              Canvas.Font := Self.Font;
              DrawCellBackground(Where, AColor, DrawState, CurCol, CurRow);
            end;
            AWhere := Where;
            if (gdPressed in DrawState) then
            begin
              Inc(AWhere.Top);
              Inc(AWhere.Left);
            end;
            if DefaultDrawing and (gdFixed in DrawState) and Ctl3D and
              ((FrameFlags1 or FrameFlags2) <> 0) and
              (FInternalDrawingStyle = gdsClassic) and not (gdPressed in DrawState) then
            begin
              TempRect := Where;
              if (FrameFlags1 and BF_RIGHT) = 0 then
                Inc(TempRect.Right, DrawInfo.Horz.EffectiveLineWidth)
              else if (FrameFlags1 and BF_BOTTOM) = 0 then
                Inc(TempRect.Bottom, DrawInfo.Vert.EffectiveLineWidth);
            end;

            if DefaultDrawing and not (csDesigning in ComponentState) and
              (gdFocused in DrawState) and
              ([goEditing, goAlwaysShowEditor] * Options <>
              [goEditing, goAlwaysShowEditor])
              and not (goRowSelect in Options) then
            begin
              TempRect := Where;
              if (FInternalDrawingStyle = gdsThemed) and (Win32MajorVersion >= 6) then
                InflateRect(TempRect, -1, -1);
              Canvas.Brush.Style := bsSolid;

              // ������ ������������
              if not UseRightToLeftAlignment then
                DrawCellHighlight(TempRect, DrawState, CurCol, CurRow)
              else
              begin
                AWhere := TempRect;
                AWhere.Left := TempRect.Right;
                AWhere.Right := TempRect.Left;
                DrawCellHighlight(Where, DrawState, CurCol, CurRow);
              end;
            end;

            DrawCell(CurCol, CurRow, Where, DrawState);
          end;
        end;
        Where.Left := Where.Right + DrawInfo.Horz.EffectiveLineWidth;
        Inc(CurCol);
      end;
      Where.Top := Where.Bottom + DrawInfo.Vert.EffectiveLineWidth;
      Inc(CurRow);
    end;
  end;

begin
  if UseRightToLeftAlignment then ChangeGridOrientation(True);

  // ��������� �����
  FInternalColor := Color;
  if FInternalDrawingStyle = gdsGradient then
    LineColor := $F0F0F0
  else
    LineColor := FLinesColor;

  UpdateRect := Canvas.ClipRect;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    if (Horz.EffectiveLineWidth > 0) or (Vert.EffectiveLineWidth > 0) then
    begin
      { Draw the grid line in the four areas (fixed, fixed), (variable, fixed),
        (fixed, variable) and (variable, variable) }
      MaxStroke := Max(Horz.LastFullVisibleCell - LeftCol {+ FixedCols} + 1,
        Vert.LastFullVisibleCell - TopRow) + 3;
      PointsList := StackAlloc(MaxStroke * sizeof(TPoint) * 2);
      StrokeList := StackAlloc(MaxStroke * sizeof(Integer));
      FillDWord(StrokeList^, MaxStroke, 2);

      if ColorToRGB(FInternalColor) = clSilver then
        LineColor := clGray;

      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        0, 0, [0, 0, Horz.FixedBoundary, Vert.FixedBoundary], LineColor, Color);

      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        LeftCol, 0, [Horz.FixedBoundary, 0, Horz.GridBoundary,
        Vert.FixedBoundary], LineColor, Color);

      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        0, TopRow, [0, Vert.FixedBoundary, Horz.FixedBoundary,
        Vert.GridBoundary], LineColor, Color);

      DrawLines(goHorzLine in Options, goVertLine in Options, LeftCol,
        TopRow, [Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridBoundary,
        Vert.GridBoundary], LineColor, FInternalColor);

      StackFree(StrokeList);
      StackFree(PointsList);
    end;

    { Draw the cells in the four areas }
    Sel := Selection;
    FrameFlags1 := 0;
    FrameFlags2 := 0;
    if goFixedVertLine in Options then
    begin
      FrameFlags1 := BF_RIGHT;
      FrameFlags2 := BF_LEFT;
    end;
    if goFixedHorzLine in Options then
    begin
      FrameFlags1 := FrameFlags1 or BF_BOTTOM;
      FrameFlags2 := FrameFlags2 or BF_TOP;
    end;

    DrawCells(0, 0, 0, 0, Horz.FixedBoundary, Vert.FixedBoundary, color,
      [gdFixed]);

    DrawCells(LeftCol, 0, Horz.FixedBoundary - FColOffset, 0, Horz.GridBoundary,  //!! clip
      Vert.FixedBoundary, color, [gdFixed]);

    DrawCells(0, TopRow, 0, Vert.FixedBoundary, Horz.FixedBoundary,
      Vert.GridBoundary, color, [gdFixed]);

    DrawCells(LeftCol, TopRow, Horz.FixedBoundary - FColOffset,                   //!! clip
      Vert.FixedBoundary, Horz.GridBoundary, Vert.GridBoundary, FInternalColor, []);

    if not (csDesigning in ComponentState) and
      (goRowSelect in Options) and DefaultDrawing and Focused then
    begin
      GridRectToScreenRect(GetSelection, FocRect, False);
      Canvas.Brush.Style := bsSolid;
      if (FInternalDrawingStyle = gdsThemed) and (Win32MajorVersion >= 6) then
        InflateRect(FocRect, -1, -1);
      AFocRect := FocRect;
      if not UseRightToLeftAlignment then
        Canvas.DrawFocusRect(AFocRect)
      else
      begin
        AFocRect := FocRect;
        AFocRect.Left := FocRect.Right;
        AFocRect.Right := FocRect.Left;
        DrawFocusRect(Canvas.Handle, AFocRect);
      end;
    end;

    { Fill in area not occupied by cells }
    if Horz.GridBoundary < Horz.GridExtent then
    begin
      Canvas.Brush.Color := FInternalColor;
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent, Vert.GridBoundary));
    end;
    if Vert.GridBoundary < Vert.GridExtent then
    begin
      Canvas.Brush.Color := FInternalColor;
      Canvas.FillRect(Rect(0, Vert.GridBoundary, Horz.GridExtent, Vert.GridExtent));
    end;
  end;

  if UseRightToLeftAlignment then ChangeGridOrientation(False);
end;

function TCustomGrid.CalcCoordFromPoint(X, Y: Integer;
  const DrawInfo: TGridDrawInfo): TGridCoord;

  function DoCalc(const AxisInfo: TGridAxisDrawInfo; N: Integer): Integer;
  var
    I, Start, Stop: Longint;
    Line: Integer;
  begin
    with AxisInfo do
    begin
      if N < FixedBoundary then
      begin
        Start := 0;
        Stop :=  FixedCellCount - 1;
        Line := 0;
      end
      else
      begin
        Start := FirstGridCell;
        Stop := GridCellCount - 1;
        Line := FixedBoundary;
      end;
      Result := -1;
      for I := Start to Stop do
      begin
        Inc(Line, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if N < Line then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
  end;

  function DoCalcRightToLeft(const AxisInfo: TGridAxisDrawInfo; N: Integer): Integer;
  var
    I, Start, Stop: Longint;
    Line: Integer;
  begin
    N := ClientWidth - N;
    with AxisInfo do
    begin
      if N < FixedBoundary then
      begin
        Start := 0;
        Stop :=  FixedCellCount - 1;
        Line := ClientWidth;
      end
      else
      begin
        Start := FirstGridCell;
        Stop := GridCellCount - 1;
        Line := FixedBoundary;
      end;
      Result := -1;
      for I := Start to Stop do
      begin
        Inc(Line, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if N < Line then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
  end;

begin
  if not UseRightToLeftAlignment then
    Result.X := DoCalc(DrawInfo.Horz, X)
  else
    Result.X := DoCalcRightToLeft(DrawInfo.Horz, X);
  Result.Y := DoCalc(DrawInfo.Vert, Y);
end;

procedure TCustomGrid.CalcDrawInfo(var DrawInfo: TGridDrawInfo);
begin
  CalcDrawInfoXY(DrawInfo, ClientWidth, ClientHeight);
end;

procedure TCustomGrid.CalcDrawInfoXY(var DrawInfo: TGridDrawInfo;
  UseWidth, UseHeight: Integer);

  procedure CalcAxis(var AxisInfo: TGridAxisDrawInfo; UseExtent: Integer);
  var
    I: Integer;
  begin
    with AxisInfo do
    begin
      GridExtent := UseExtent;
      GridBoundary := FixedBoundary;
      FullVisBoundary := FixedBoundary;
      LastFullVisibleCell := FirstGridCell;
      for I := FirstGridCell to GridCellCount - 1 do
      begin
        Inc(GridBoundary, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if GridBoundary > GridExtent + EffectiveLineWidth then
        begin
          GridBoundary := GridExtent;
          Break;
        end;
        LastFullVisibleCell := I;
        FullVisBoundary := GridBoundary;
      end;
    end;
  end;

begin
  CalcFixedInfo(DrawInfo);
  CalcAxis(DrawInfo.Horz, UseWidth);
  CalcAxis(DrawInfo.Vert, UseHeight);
end;

procedure TCustomGrid.CalcFixedInfo(var DrawInfo: TGridDrawInfo);

  procedure CalcFixedAxis(var Axis: TGridAxisDrawInfo; LineOptions: TGridOptions;
    FixedCount, FirstCell, CellCount: Integer; GetExtentFunc: TGetExtentsFunc);
  var
    I: Integer;
  begin
    with Axis do
    begin
      if LineOptions * Options = [] then
        EffectiveLineWidth := 0
      else
        EffectiveLineWidth := GridLineWidth;

      FixedBoundary := 0;
      for I := 0 to FixedCount - 1 do
        Inc(FixedBoundary, GetExtentFunc(I) + EffectiveLineWidth);

      FixedCellCount := FixedCount;
      FirstGridCell := FirstCell;
      GridCellCount := CellCount;
      GetExtent := GetExtentFunc;
    end;
  end;

begin
  CalcFixedAxis(DrawInfo.Horz, [goFixedVertLine, goVertLine], {FixedCols}1,
    LeftCol, ColCount, GetColWidths);
  CalcFixedAxis(DrawInfo.Vert, [goFixedHorzLine, goHorzLine], 0,
    TopRow, RowCount, GetRowHeights);
end;

{ Calculates the TopLeft that will put the given Coord in view }
function TCustomGrid.CalcMaxTopLeft(const Coord: TGridCoord;
  const DrawInfo: TGridDrawInfo): TGridCoord;

  function CalcMaxCell(const Axis: TGridAxisDrawInfo; Start: Integer): Integer;
  var
    Line: Integer;
    I, Extent: Longint;
  begin
    Result := Start;
    with Axis do
    begin
      Line := GridExtent + EffectiveLineWidth;
      for I := Start downto FixedCellCount do
      begin
        Extent := GetExtent(I);
        if Extent > 0 then
        begin
          Dec(Line, Extent);
          Dec(Line, EffectiveLineWidth);
          if Line < FixedBoundary then
          begin
            if (Result = Start) and (GetExtent(Start) <= 0) then
              Result := I;
            Break;
          end;
          Result := I;
        end;
      end;
    end;
  end;

begin
  Result.X := CalcMaxCell(DrawInfo.Horz, Coord.X);
  Result.Y := CalcMaxCell(DrawInfo.Vert, Coord.Y);
end;

procedure TCustomGrid.CalcSizingState(X, Y: Integer; var State: TGridState;
  var Index: Longint; var SizingPos, SizingOfs: Integer;
  var FixedInfo: TGridDrawInfo);

  procedure CalcAxisState(const AxisInfo: TGridAxisDrawInfo; Pos: Integer;
    NewState: TGridState);
  var
    I, Line, Back, Range: Integer;
  begin
    if (NewState = gsColSizing) and UseRightToLeftAlignment then
      Pos := ClientWidth - Pos;
    with AxisInfo do
    begin
      Line := FixedBoundary;
      Range := EffectiveLineWidth;
      Back := 0;
      if Range < 7 then
      begin
        Range := 7;
        Back := (Range - EffectiveLineWidth) shr 1;
      end;
      for I := FirstGridCell to GridCellCount - 1 do
      begin
        Inc(Line, AxisInfo.GetExtent(I));
        if Line > GridBoundary then Break;
        if (Pos >= Line - Back) and (Pos <= Line - Back + Range) then
        begin
          State := NewState;
          SizingPos := Line;
          SizingOfs := Line - Pos;
          Index := I;
          Exit;
        end;
        Inc(Line, EffectiveLineWidth);
      end;
      if (GridBoundary = GridExtent) and (Pos >= GridExtent - Back)
        and (Pos <= GridExtent) then
      begin
        State := NewState;
        SizingPos := GridExtent;
        SizingOfs := GridExtent - Pos;
        Index := LastFullVisibleCell + 1;
      end;
    end;
  end;

  function XOutsideHorzFixedBoundary: Boolean;
  begin
    with FixedInfo do
      if not UseRightToLeftAlignment then
        Result := X > Horz.FixedBoundary
      else
        Result := X < ClientWidth - Horz.FixedBoundary;
  end;

var
  EffectiveOptions: TGridOptions;
begin
  State := gsNormal;
  Index := -1;
  EffectiveOptions := Options;
  if csDesigning in ComponentState then
    EffectiveOptions := EffectiveOptions + DesignOptionsBoost;
  if [goColSizing, goRowSizing] * EffectiveOptions <> [] then
    with FixedInfo do
    begin
      Vert.GridExtent := ClientHeight;
      Horz.GridExtent := ClientWidth;
      if (XOutsideHorzFixedBoundary) and
         (goColSizing in EffectiveOptions) then
        CalcAxisState(Horz, X, gsColSizing);
    end;
end;

procedure TCustomGrid.ChangeGridOrientation(RightToLeftOrientation: Boolean);
var
  Org: TPoint;
  Ext: TPoint;
begin
  if RightToLeftOrientation then
  begin
    Org := Point(ClientWidth,0);
    Ext := Point(-1,1);
    SetMapMode(Canvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(Canvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(Canvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end
  else
  begin
    Org := Point(0,0);
    Ext := Point(1,1);
    SetMapMode(Canvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(Canvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(Canvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end;
end;

procedure TCustomGrid.ChangeSize(NewColCount, NewRowCount: Longint);
var
  OldColCount, OldRowCount: Longint;
  OldDrawInfo: TGridDrawInfo;

  procedure MinRedraw(const OldInfo, NewInfo: TGridAxisDrawInfo; Axis: Integer);
  var
    R: TRect;
    First: Integer;
  begin
    First := Min(OldInfo.LastFullVisibleCell, NewInfo.LastFullVisibleCell);
    // Get the rectangle around the leftmost or topmost cell in the target range.
    R := CellRect(First and not Axis, First and Axis);
    R.Bottom := Height;
    R.Right := Width;
    Windows.InvalidateRect(Handle, R, False);
  end;

  procedure DoChange;
  var
    Coord: TGridCoord;
    NewDrawInfo: TGridDrawInfo;
  begin
    if FColWidths <> nil then
      UpdateExtents(FColWidths, ColCount, DefaultColWidth);
    if FTabStops <> nil then
      UpdateExtents(FTabStops, ColCount, Integer(True));
    if FRowHeights <> nil then
      UpdateExtents(FRowHeights, RowCount, DefaultRowHeight);
    Coord := FCurrent;
    if Row >= RowCount then Coord.Y := RowCount - 1;
    if Col >= ColCount then Coord.X := ColCount - 1;
    if (FCurrent.X <> Coord.X) or (FCurrent.Y <> Coord.Y) then
      MoveCurrent(Coord.X, Coord.Y, True, True);
    if (FAnchor.X <> Coord.X) or (FAnchor.Y <> Coord.Y) then
      MoveAnchor(Coord);
    if VirtualView or
      (LeftCol <> OldDrawInfo.Horz.FirstGridCell) or
      (TopRow <> OldDrawInfo.Vert.FirstGridCell) then
      InvalidateGrid
    else if HandleAllocated then
    begin
      CalcDrawInfo(NewDrawInfo);
      MinRedraw(OldDrawInfo.Horz, NewDrawInfo.Horz, 0);
      MinRedraw(OldDrawInfo.Vert, NewDrawInfo.Vert, -1);
    end;
    UpdateScrollRange;
    SizeChanged(OldColCount, OldRowCount);
  end;

begin
  if HandleAllocated then
    CalcDrawInfo(OldDrawInfo);
  OldColCount := FColCount;
  OldRowCount := FRowCount;
  FColCount := NewColCount;
  FRowCount := NewRowCount;
//  if FixedCols > NewColCount then FFixedCols := NewColCount - 1;
//  if FixedRows > NewRowCount then FFixedRows := NewRowCount - 1;
  try
    DoChange;
  except
    { Could not change size so try to clean up by setting the size back }
    FColCount := OldColCount;
    FRowCount := OldRowCount;
    DoChange;
    InvalidateGrid;
    raise;
  end;
end;

{ Will move TopLeft so that Coord is in view }
procedure TCustomGrid.ClampInView(const Coord: TGridCoord);
var
  DrawInfo: TGridDrawInfo;
  MaxTopLeft: TGridCoord;
  OldTopLeft: TGridCoord;
begin
  if not HandleAllocated then Exit;
  CalcDrawInfo(DrawInfo);
  with DrawInfo, Coord do
  begin
    if (X > Horz.LastFullVisibleCell) or
      (Y > Vert.LastFullVisibleCell) or (X < LeftCol) or (Y < TopRow) then
    begin
      OldTopLeft := FTopLeft;
      MaxTopLeft := CalcMaxTopLeft(Coord, DrawInfo);
      Update;
      if X < LeftCol then FTopLeft.X := X
      else if X > Horz.LastFullVisibleCell then FTopLeft.X := MaxTopLeft.X;
      if Y < TopRow then FTopLeft.Y := Y
      else if Y > Vert.LastFullVisibleCell then FTopLeft.Y := MaxTopLeft.Y;
      TopLeftMoved(OldTopLeft);
    end;
  end;
end;

procedure TCustomGrid.DrawCellHighlight(const ARect: TRect; AState: TGridDrawState;
  ACol, ARow: Integer);
var
  LRect: TRect;
//  LTheme: HTHEME;
  LColor: TColorRef;
begin
  if (goRowSelect in Options) then
    Include(AState, gdRowSelected);

  if FInternalDrawingStyle = gdsGradient then
  begin
    LRect := ARect;
    Canvas.Brush.Color := FHighlightingColor;
    Canvas.FrameRect(LRect);
    if (gdRowSelected in AState) then
    begin
      InflateRect(LRect, 0, -1);
      if (ACol >= {FixedCols +} 2) and (ACol < ColCount - 1) then
        InflateRect(LRect, 2, 0)
      else if ACol = 1{FixedCols} then
        Inc(LRect.Left)
      else if ACol = (ColCount - 1) then
        Dec(LRect.Right);
    end
    else
      InflateRect(LRect, -1, -1);
    GradientFillCanvas(Canvas, GetShadowColor(FHighlightingColor, 45),
      GetShadowColor(FHighlightingColor, 10), LRect, gdVertical);
    Canvas.Brush.Style := bsClear;
  end
  else
  begin
    Canvas.Brush.Color := FHighlightingColor;
    Canvas.FillRect(ARect);
  end;
end;

procedure TCustomGrid.DrawCellBackground(const ARect: TRect; AColor: TColor;
  AState: TGridDrawState; ACol, ARow: Integer);
var
  LRect, ClipRect: TRect;
begin
  LRect := ARect;

  if (FInternalDrawingStyle = gdsGradient) and (gdFixed in AState) then
  begin
    if not (goFixedVertLine in Options) then
      Inc(LRect.Right);
    if not (goFixedHorzLine in Options) then
      Inc(LRect.Bottom);

    if (gdHotTrack in AState) or (gdPressed in AState) then
    begin
      if (gdPressed in AState) then
        GradientFillCanvas(Canvas, FGradientEndColor, FGradientStartColor, LRect, gdVertical)
      else
        GradientFillCanvas(Canvas, GetHighlightColor(FGradientStartColor),
          GetHighlightColor(FGradientEndColor), LRect, gdVertical);
    end
    else
      GradientFillCanvas(Canvas, FGradientStartColor, FGradientEndColor, LRect, gdVertical);
    Canvas.Brush.Style := bsClear;
  end
  else
  begin
    Canvas.Brush.Color := AColor;
    Canvas.FillRect(LRect);
    if (gdPressed in AState) then
    begin
      Dec(LRect.Right);
      Dec(LRect.Bottom);
      DrawEdge(Canvas.Handle, LRect, BDR_SUNKENINNER, BF_TOPLEFT);
      DrawEdge(Canvas.Handle, LRect, BDR_SUNKENINNER, BF_BOTTOMRIGHT);
    end;
  end;
end;

procedure TCustomGrid.DrawMove;
var
  OldPen: TPen;
  Pos: Integer;
  R: TRect;
begin
  OldPen := TPen.Create;
  try
    with Canvas do
    begin
      OldPen.Assign(Pen);
      try
        Pen.Style := psDot;
        Pen.Mode := pmXor;
        Pen.Width := 5;
        if FGridState = gsRowMoving then
        begin
          R := CellRect(0, FMovePos);
          if FMovePos > FMoveIndex then
            Pos := R.Bottom else
            Pos := R.Top;
          MoveTo(0, Pos);
          LineTo(ClientWidth, Pos);
        end
        else
        begin
          R := CellRect(FMovePos, 0);
          if FMovePos > FMoveIndex then
            if not UseRightToLeftAlignment then
              Pos := R.Right
            else
              Pos := R.Left
          else
            if not UseRightToLeftAlignment then
              Pos := R.Left
            else
              Pos := R.Right;
          MoveTo(Pos, 0);
          LineTo(Pos, ClientHeight);
        end;
      finally
        Canvas.Pen := OldPen;
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TCustomGrid.FixedCellClick(ACol, ARow: Integer);
begin
  if Assigned(FOnFixedCellClick) then
    FOnFixedCellClick(Self, ACol, ARow);
end;

procedure TCustomGrid.FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
begin
  MoveCurrent(ACol, ARow, MoveAnchor, True);
  UpdateEdit;
  Click;
end;

procedure TCustomGrid.GridRectToScreenRect(GridRect: TGridRect;
  var ScreenRect: TRect; IncludeLine: Boolean);

  function LinePos(const AxisInfo: TGridAxisDrawInfo; Line: Integer): Integer;
  var
    Start, I: Longint;
  begin
    with AxisInfo do
    begin
      Result := 0;
      if Line < FixedCellCount then
        Start := 0
      else
      begin
        if Line >= FirstGridCell then
          Result := FixedBoundary;
        Start := FirstGridCell;
      end;
      for I := Start to Line - 1 do
      begin
        Inc(Result, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if Result > GridExtent then
        begin
          Result := 0;
          Exit;
        end;
      end;
    end;
  end;

  function CalcAxis(const AxisInfo: TGridAxisDrawInfo;
    GridRectMin, GridRectMax: Integer;
    var ScreenRectMin, ScreenRectMax: Integer): Boolean;
  begin
    Result := False;
    with AxisInfo do
    begin
      if (GridRectMin >= FixedCellCount) and (GridRectMin < FirstGridCell) then
        if GridRectMax < FirstGridCell then
        begin
          ScreenRect := Rect(0, 0, 0, 0); { erase partial results}
          Exit;
        end
        else
          GridRectMin := FirstGridCell;
      if GridRectMax > LastFullVisibleCell then
      begin
        GridRectMax := LastFullVisibleCell;
        if GridRectMax < GridCellCount - 1 then Inc(GridRectMax);
        if LinePos(AxisInfo, GridRectMax) = 0 then
          Dec(GridRectMax);
      end;

      ScreenRectMin := LinePos(AxisInfo, GridRectMin);
      ScreenRectMax := LinePos(AxisInfo, GridRectMax);
      if ScreenRectMax = 0 then
        ScreenRectMax := ScreenRectMin + AxisInfo.GetExtent(GridRectMin)
      else
        Inc(ScreenRectMax, AxisInfo.GetExtent(GridRectMax));
      if ScreenRectMax > GridExtent then
        ScreenRectMax := GridExtent;
      if IncludeLine then Inc(ScreenRectMax, EffectiveLineWidth);
    end;
    Result := True;
  end;

var
  DrawInfo: TGridDrawInfo;
  Hold: Integer;
begin
  ScreenRect := Rect(0, 0, 0, 0);
  if (GridRect.Left > GridRect.Right) or (GridRect.Top > GridRect.Bottom) then
    Exit;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    if GridRect.Left > Horz.LastFullVisibleCell + 1 then Exit;
    if GridRect.Top > Vert.LastFullVisibleCell + 1 then Exit;

    if CalcAxis(Horz, GridRect.Left, GridRect.Right, ScreenRect.Left,
      ScreenRect.Right) then
    begin
      CalcAxis(Vert, GridRect.Top, GridRect.Bottom, ScreenRect.Top,
        ScreenRect.Bottom);
    end;
  end;
  if UseRightToLeftAlignment and (Canvas.CanvasOrientation = coLeftToRight) then
  begin
    Hold := ScreenRect.Left;
    ScreenRect.Left := ClientWidth - ScreenRect.Right;
    ScreenRect.Right := ClientWidth - Hold;
  end;
end;

procedure TCustomGrid.Initialize;
begin
  FTopLeft.X := {FixedCols}1;
  FTopLeft.Y := {FixedRows}0;
  FCurrent := FTopLeft;
  FAnchor := FCurrent;
  if goRowSelect in Options then
    FAnchor.X := ColCount - 1;
end;

procedure TCustomGrid.InvalidateCell(ACol, ARow: Longint);
var
  Rect: TGridRect;
begin
  Rect.Top := ARow;
  Rect.Left := ACol;
  Rect.Bottom := ARow;
  Rect.Right := ACol;
  InvalidateRect(Rect);
end;

procedure TCustomGrid.InvalidateCol(ACol: Longint);
var
  Rect: TGridRect;
begin
  if not HandleAllocated then Exit;
  Rect.Top := 0;
  Rect.Left := ACol;
  Rect.Bottom := VisibleRowCount+1;
  Rect.Right := ACol;
  InvalidateRect(Rect);
end;

procedure TCustomGrid.InvalidateRow(ARow: Longint);
var
  Rect: TGridRect;
begin
  if not HandleAllocated then Exit;
  Rect.Top := ARow;
  Rect.Left := 0;
  Rect.Bottom := ARow;
  Rect.Right := VisibleColCount+1;
  InvalidateRect(Rect);
end;

procedure TCustomGrid.InvalidateGrid;
begin
  Invalidate;
end;

procedure TCustomGrid.InvalidateRect(ARect: TGridRect);
var
  InvalidRect: TRect;
begin
  if not HandleAllocated then Exit;
  GridRectToScreenRect(ARect, InvalidRect, True);
  Windows.InvalidateRect(Handle, InvalidRect, False);
end;

function TCustomGrid.IsTouchPropertyStored(AProperty: TTouchProperty): Boolean;
begin
  Result := inherited IsTouchPropertyStored(AProperty);
  case AProperty of
    tpInteractiveGestures:
      Result := Touch.InteractiveGestures <> [igPan, igPressAndTap];
    tpInteractiveGestureOptions:
      Result := Touch.InteractiveGestureOptions <> [igoPanInertia,
        igoPanSingleFingerHorizontal, igoPanSingleFingerVertical,
        igoPanGutter, igoParentPassthrough];
  end;
end;

procedure TCustomGrid.ModifyScrollBar(ScrollBar, ScrollCode, Pos: Cardinal;
  UseRightToLeft: Boolean);
var
  NewTopLeft, MaxTopLeft: TGridCoord;
  DrawInfo: TGridDrawInfo;
  RTLFactor: Integer;

  function Min: Longint;
  begin
    if ScrollBar = SB_HORZ then Result := 1//FixedCols
    else Result := 0//FixedRows;
  end;

  function Max: Longint;
  begin
    if ScrollBar = SB_HORZ then Result := MaxTopLeft.X
    else Result := MaxTopLeft.Y;
  end;

  function PageUp: Longint;
  var
    MaxTopLeft: TGridCoord;
  begin
    MaxTopLeft := CalcMaxTopLeft(FTopLeft, DrawInfo);
    if ScrollBar = SB_HORZ then
      Result := FTopLeft.X - MaxTopLeft.X else
      Result := FTopLeft.Y - MaxTopLeft.Y;
    if Result < 1 then Result := 1;
  end;

  function PageDown: Longint;
  var
    DrawInfo: TGridDrawInfo;
  begin
    CalcDrawInfo(DrawInfo);
    with DrawInfo do
      if ScrollBar = SB_HORZ then
        Result := Horz.LastFullVisibleCell - FTopLeft.X else
        Result := Vert.LastFullVisibleCell - FTopLeft.Y;
    if Result < 1 then Result := 1;
  end;

  function CalcScrollBar(Value, ARTLFactor: Longint): Longint;
  begin
    Result := Value;
    case ScrollCode of
      SB_LINEUP:
        Dec(Result, ARTLFactor);
      SB_LINEDOWN:
        Inc(Result, ARTLFactor);
      SB_PAGEUP:
        Dec(Result, PageUp * ARTLFactor);
      SB_PAGEDOWN:
        Inc(Result, PageDown * ARTLFactor);
      SB_THUMBPOSITION, SB_THUMBTRACK:
        if (goThumbTracking in Options) or (ScrollCode = SB_THUMBPOSITION) then
        begin
          if (not UseRightToLeftAlignment) or (ARTLFactor = 1) then
            Result := Min + LongMulDiv(Pos, Max - Min, MaxShortInt)
          else
            Result := Max - LongMulDiv(Pos, Max - Min, MaxShortInt);
        end;
      SB_BOTTOM:
        Result := Max;
      SB_TOP:
        Result := Min;
    end;
  end;

  procedure ModifyPixelScrollBar(Code, Pos: Cardinal);
  var
    NewOffset: Integer;
    OldOffset: Integer;
    R: TGridRect;
    GridSpace, ColWidth: Integer;
  begin
    NewOffset := FColOffset;
    ColWidth := ColWidths[DrawInfo.Horz.FirstGridCell];
    GridSpace := ClientWidth - DrawInfo.Horz.FixedBoundary;
    case Code of
      SB_LINEUP: Dec(NewOffset, Canvas.TextWidth('0') * RTLFactor);
      SB_LINEDOWN: Inc(NewOffset, Canvas.TextWidth('0') * RTLFactor);
      SB_PAGEUP: Dec(NewOffset, GridSpace * RTLFactor);
      SB_PAGEDOWN: Inc(NewOffset, GridSpace * RTLFactor);
      SB_THUMBPOSITION,
      SB_THUMBTRACK:
        if (goThumbTracking in Options) or (Code = SB_THUMBPOSITION) then
        begin
          if not UseRightToLeftAlignment then
            NewOffset := Pos
          else
            NewOffset := Max - Integer(Pos);
        end;
      SB_BOTTOM: NewOffset := 0;
      SB_TOP: NewOffset := ColWidth - GridSpace;
    end;
    if NewOffset < 0 then
      NewOffset := 0
    else if NewOffset >= ColWidth - GridSpace then
      NewOffset := ColWidth - GridSpace;
    if NewOffset <> FColOffset then
    begin
      OldOffset := FColOffset;
      FColOffset := NewOffset;
      ScrollData(OldOffset - NewOffset, 0);
      FillChar(R, SizeOf(R), 0);
      R.Bottom := 0{FixedRows};
      InvalidateRect(R);
      Update;
      UpdateScrollPos;
    end;
  end;

var
  Temp: Longint;
begin
  if (not UseRightToLeftAlignment) or (not UseRightToLeft) then
    RTLFactor := 1
  else
    RTLFactor := -1;
  if Visible and CanFocus and TabStop and not (csDesigning in ComponentState) then
    SetFocus;
  CalcDrawInfo(DrawInfo);
  if (ScrollBar = SB_HORZ) and (ColCount = 1) then
  begin
    ModifyPixelScrollBar(ScrollCode, Pos);
    Exit;
  end;
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  NewTopLeft := FTopLeft;
  if ScrollBar = SB_HORZ then
    repeat
      Temp := NewTopLeft.X;
      NewTopLeft.X := CalcScrollBar(NewTopLeft.X, RTLFactor);
    until (NewTopLeft.X <= 1{FixedCols}) or (NewTopLeft.X >= MaxTopLeft.X)
      or (ColWidths[NewTopLeft.X] > 0) or (Temp = NewTopLeft.X)
  else
    repeat
      Temp := NewTopLeft.Y;
      NewTopLeft.Y := CalcScrollBar(NewTopLeft.Y, 1);
    until (NewTopLeft.Y <= {FixedRows}0) or (NewTopLeft.Y >= MaxTopLeft.Y)
      or (RowHeights[NewTopLeft.Y] > 0) or (Temp = NewTopLeft.Y);
  NewTopLeft.X := Math.Max(1{FixedCols}, Math.Min(MaxTopLeft.X, NewTopLeft.X));
  NewTopLeft.Y := Math.Max({FixedRows}0, Math.Min(MaxTopLeft.Y, NewTopLeft.Y));
  if (NewTopLeft.X <> FTopLeft.X) or (NewTopLeft.Y <> FTopLeft.Y) then
    MoveTopLeft(NewTopLeft.X, NewTopLeft.Y);
end;

procedure TCustomGrid.MoveAdjust(var CellPos: Longint; FromIndex, ToIndex: Longint);
var
  Min, Max: Longint;
begin
  if CellPos = FromIndex then CellPos := ToIndex
  else
  begin
    Min := FromIndex;
    Max := ToIndex;
    if FromIndex > ToIndex then
    begin
      Min := ToIndex;
      Max := FromIndex;
    end;
    if (CellPos >= Min) and (CellPos <= Max) then
      if FromIndex > ToIndex then
        Inc(CellPos) else
        Dec(CellPos);
  end;
end;

procedure TCustomGrid.MoveAnchor(const NewAnchor: TGridCoord);
var
  OldSel: TGridRect;
begin
  if [goRangeSelect, goEditing] * Options = [goRangeSelect] then
  begin
    OldSel := Selection;
    FAnchor := NewAnchor;
    if goRowSelect in Options then FAnchor.X := ColCount - 1;
    ClampInView(NewAnchor);
    SelectionMoved(OldSel);
  end
  else MoveCurrent(NewAnchor.X, NewAnchor.Y, True, True);
end;

procedure TCustomGrid.MoveCurrent(ACol, ARow: Longint; MoveAnchor,
  Show: Boolean);
var
  OldSel: TGridRect;
  OldCurrent: TGridCoord;
begin
  if (ACol < 0) or (ARow < 0) or (ACol >= ColCount) or (ARow >= RowCount) then
    InvalidOp(SIndexOutOfRange);
  if SelectCell(ACol, ARow) then
  begin
    OldSel := Selection;
    OldCurrent := FCurrent;
    FCurrent.X := ACol;
    FCurrent.Y := ARow;
    if not (goAlwaysShowEditor in Options) then HideEditor;
    if MoveAnchor or not (goRangeSelect in Options) then
    begin
      FAnchor := FCurrent;
      if goRowSelect in Options then FAnchor.X := ColCount - 1;
    end;
    if goRowSelect in Options then FCurrent.X := 1{FixedCols};
    if Show then ClampInView(FCurrent);
    SelectionMoved(OldSel);
    with OldCurrent do InvalidateCell(X, Y);
    with FCurrent do InvalidateCell(ACol, ARow);
  end;
end;

procedure TCustomGrid.MoveTopLeft(ALeft, ATop: Longint);
var
  OldTopLeft: TGridCoord;
begin
  if (ALeft = FTopLeft.X) and (ATop = FTopLeft.Y) then Exit;
  Update;
  OldTopLeft := FTopLeft;
  FTopLeft.X := ALeft;
  FTopLeft.Y := ATop;
  TopLeftMoved(OldTopLeft);
end;

procedure TCustomGrid.ResizeCol(Index: Longint; OldSize, NewSize: Integer);
begin
  InvalidateGrid;
end;

procedure TCustomGrid.ResizeRow(Index: Longint; OldSize, NewSize: Integer);
begin
  InvalidateGrid;
end;

procedure TCustomGrid.SelectionMoved(const OldSel: TGridRect);
var
  OldRect, NewRect: TRect;
  AXorRects: TXorRects;
  I: Integer;
begin
  if not HandleAllocated then Exit;
  GridRectToScreenRect(OldSel, OldRect, True);
  GridRectToScreenRect(Selection, NewRect, True);
  XorRects(OldRect, NewRect, AXorRects);
  for I := Low(AXorRects) to High(AXorRects) do
    Windows.InvalidateRect(Handle, AXorRects[I], False);
end;

procedure TCustomGrid.ScrollDataInfo(DX, DY: Integer;
  var DrawInfo: TGridDrawInfo);
var
  ScrollArea: TRect;
  ScrollFlags: Integer;
begin
  with DrawInfo do
  begin
    ScrollFlags := SW_INVALIDATE;
    if not DefaultDrawing then
      ScrollFlags := ScrollFlags or SW_ERASE;
    { Scroll the area }
    if DY = 0 then
    begin
      { Scroll both the column titles and data area at the same time }
      if not UseRightToLeftAlignment then
        ScrollArea := Rect(Horz.FixedBoundary, 0, Horz.GridExtent, Vert.GridExtent)
      else
      begin
        ScrollArea := Rect(ClientWidth - Horz.GridExtent, 0, ClientWidth - Horz.FixedBoundary, Vert.GridExtent);
        DX := -DX;
      end;
      ScrollWindowEx(Handle, DX, 0, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
    end
    else if DX = 0 then
    begin
      { Scroll both the row titles and data area at the same time }
      ScrollArea := Rect(0, Vert.FixedBoundary, Horz.GridExtent, Vert.GridExtent);
      ScrollWindowEx(Handle, 0, DY, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
    end
    else
    begin
      { Scroll titles and data area separately }
      { Column titles }
      ScrollArea := Rect(Horz.FixedBoundary, 0, Horz.GridExtent, Vert.FixedBoundary);
      ScrollWindowEx(Handle, DX, 0, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
      { Row titles }
      ScrollArea := Rect(0, Vert.FixedBoundary, Horz.FixedBoundary, Vert.GridExtent);
      ScrollWindowEx(Handle, 0, DY, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
      { Data area }
      ScrollArea := Rect(Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridExtent,
        Vert.GridExtent);
      ScrollWindowEx(Handle, DX, DY, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
    end;
  end;
  if goRowSelect in Options then
    InvalidateRect(Selection);
end;

procedure TCustomGrid.ScrollData(DX, DY: Integer);
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  ScrollDataInfo(DX, DY, DrawInfo);
end;

procedure TCustomGrid.TopLeftMoved(const OldTopLeft: TGridCoord);

  function CalcScroll(const AxisInfo: TGridAxisDrawInfo;
    OldPos, CurrentPos: Integer; var Amount: Longint): Boolean;
  var
    Start, Stop: Longint;
    I: Longint;
  begin
    Result := False;
    with AxisInfo do
    begin
      if OldPos < CurrentPos then
      begin
        Start := OldPos;
        Stop := CurrentPos;
      end
      else
      begin
        Start := CurrentPos;
        Stop := OldPos;
      end;
      Amount := 0;
      for I := Start to Stop - 1 do
      begin
        Inc(Amount, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if Amount > (GridBoundary - FixedBoundary) then
        begin
          { Scroll amount too big, redraw the whole thing }
          InvalidateGrid;
          Exit;
        end;
      end;
      if OldPos < CurrentPos then Amount := -Amount;
    end;
    Result := True;
  end;

var
  DrawInfo: TGridDrawInfo;
  Delta: TGridCoord;
begin
  UpdateScrollPos;
  CalcDrawInfo(DrawInfo);
  if CalcScroll(DrawInfo.Horz, OldTopLeft.X, FTopLeft.X, Delta.X) and
    CalcScroll(DrawInfo.Vert, OldTopLeft.Y, FTopLeft.Y, Delta.Y) then
    ScrollDataInfo(Delta.X, Delta.Y, DrawInfo);
  TopLeftChanged;
end;

procedure TCustomGrid.UpdateScrollPos;
var
  DrawInfo: TGridDrawInfo;
  MaxTopLeft: TGridCoord;
  GridSpace, ColWidth: Integer;

  procedure SetScroll(Code: Word; Value: Integer);
  begin
    if UseRightToLeftAlignment and (Code = SB_HORZ) then
      if ColCount <> 1 then Value := MaxShortInt - Value
      else                  Value := (ColWidth - GridSpace) - Value;
    if GetScrollPos(Handle, Code) <> Value then
      SetScrollPos(Handle, Code, Value, True);
  end;

begin
  if (not HandleAllocated) or (ScrollBars = ssNone) then Exit;
  CalcDrawInfo(DrawInfo);
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  if ScrollBars in [ssHorizontal, ssBoth] then
    if ColCount = 1 then
    begin
      ColWidth := ColWidths[DrawInfo.Horz.FirstGridCell];
      GridSpace := ClientWidth - DrawInfo.Horz.FixedBoundary;
      if (FColOffset > 0) and (GridSpace > (ColWidth - FColOffset)) then
        ModifyScrollbar(SB_HORZ, SB_THUMBPOSITION, ColWidth - GridSpace, True)
      else
        SetScroll(SB_HORZ, FColOffset)
    end
    else
      SetScroll(SB_HORZ, LongMulDiv(FTopLeft.X - 1{ - FixedCols}, MaxShortInt,
        MaxTopLeft.X -1{- FixedCols}));
  if ScrollBars in [ssVertical, ssBoth] then
    SetScroll(SB_VERT, LongMulDiv(FTopLeft.Y {- FixedRows}, MaxShortInt,
      MaxTopLeft.Y{ - FixedRows}));
end;

procedure TCustomGrid.UpdateScrollRange;
var
  MaxTopLeft, OldTopLeft: TGridCoord;
  DrawInfo: TGridDrawInfo;
  OldScrollBars: TScrollStyle;
  Updated: Boolean;

  procedure DoUpdate;
  begin
    if not Updated then
    begin
      Update;
      Updated := True;
    end;
  end;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Min, Max: Integer;
  begin
    Result := False;
    if (ScrollBars = ssBoth) or
      ((Code = SB_HORZ) and (ScrollBars = ssHorizontal)) or
      ((Code = SB_VERT) and (ScrollBars = ssVertical)) then
    begin
      GetScrollRange(Handle, Code, Min, Max);
      Result := Min <> Max;
    end;
  end;

  procedure CalcSizeInfo;
  begin
    CalcDrawInfoXY(DrawInfo, DrawInfo.Horz.GridExtent, DrawInfo.Vert.GridExtent);
    MaxTopLeft.X := ColCount - 1;
    MaxTopLeft.Y := RowCount - 1;
    MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  end;

  procedure SetAxisRange(var Max, Old, Current: Longint; Code: Word;
    Fixeds: Integer);
  begin
    CalcSizeInfo;
    if Fixeds < Max then
      SetScrollRange(Handle, Code, 0, MaxShortInt, True)
    else
      SetScrollRange(Handle, Code, 0, 0, True);
    if Old > Max then
    begin
      DoUpdate;
      Current := Max;
    end;
  end;

  procedure SetHorzRange;
  var
    Range: Integer;
  begin
    if OldScrollBars in [ssHorizontal, ssBoth] then
      if ColCount = 1 then
      begin
        Range := ColWidths[0] - ClientWidth;
        if Range < 0 then Range := 0;
        SetScrollRange(Handle, SB_HORZ, 0, Range, True);
      end
      else
        SetAxisRange(MaxTopLeft.X, OldTopLeft.X, FTopLeft.X, SB_HORZ, {FixedCols}1);
  end;

  procedure SetVertRange;
  begin
    if OldScrollBars in [ssVertical, ssBoth] then
      SetAxisRange(MaxTopLeft.Y, OldTopLeft.Y, FTopLeft.Y, SB_VERT, {FixedRows}0);
  end;

begin
  if (ScrollBars = ssNone) or not HandleAllocated or not Showing then Exit;
  with DrawInfo do
  begin
    Horz.GridExtent := ClientWidth;
    Vert.GridExtent := ClientHeight;
    { Ignore scroll bars for initial calculation }
    if ScrollBarVisible(SB_HORZ) then
      Inc(Vert.GridExtent, GetSystemMetrics(SM_CYHSCROLL));
    if ScrollBarVisible(SB_VERT) then
      Inc(Horz.GridExtent, GetSystemMetrics(SM_CXVSCROLL));
  end;
  OldTopLeft := FTopLeft;
  { Temporarily mark us as not having scroll bars to avoid recursion }
  OldScrollBars := FScrollBars;
  FScrollBars := ssNone;
  Updated := False;
  try
    { Update scrollbars }
    SetHorzRange;
    DrawInfo.Vert.GridExtent := ClientHeight;
    SetVertRange;
    if DrawInfo.Horz.GridExtent <> ClientWidth then
    begin
      DrawInfo.Horz.GridExtent := ClientWidth;
      SetHorzRange;
    end;
  finally
    FScrollBars := OldScrollBars;
  end;
  UpdateScrollPos;
  if (FTopLeft.X <> OldTopLeft.X) or (FTopLeft.Y <> OldTopLeft.Y) then
    TopLeftMoved(OldTopLeft);
end;

function TCustomGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEdit.Create(Self);
end;

procedure TCustomGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
    WindowClass.style := CS_DBLCLKS;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TCustomGrid.CreateWnd;
begin
  inherited;
//  FInternalDrawingStyle := FDrawingStyle;
//  if (FDrawingStyle = gdsThemed) {and not ThemeControl(Self)} then
    FInternalDrawingStyle := gdsClassic;
end;

procedure TCustomGrid.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
const
  VertScrollFlags: array[Boolean] of Integer = (SB_LINEDOWN, SB_LINEUP);
  HorizScrollFlags: array[Boolean] of Integer = (SB_LINERIGHT, SB_LINELEFT);
var
  I, LColWidth, LCols, LRowHeight, LRows, DeltaX, DeltaY: Integer;
begin
  if EventInfo.GestureID = igiPan then
  begin
    Handled := True;
    if gfBegin in EventInfo.Flags then
      FPanPoint := EventInfo.Location
    else if not (gfEnd in EventInfo.Flags) then
    begin
      // Vertical panning
      DeltaY := EventInfo.Location.Y - FPanPoint.Y;
      if Abs(DeltaY) > 1 then
      begin
        LRowHeight := RowHeights[TopRow];
        LRows := Abs(DeltaY) div LRowHeight;
        if (Abs(DeltaY) mod LRowHeight = 0) or (LRows > 0) then
        begin
          for I := 0 to LRows - 1 do
            ModifyScrollBar(SB_VERT, VertScrollFlags[DeltaY > 0], 0, True);
          FPanPoint := EventInfo.Location;
          Inc(FPanPoint.Y, DeltaY mod LRowHeight);
        end;
      end
      else
      begin
        // Horizontal panning
        DeltaX := EventInfo.Location.X - FPanPoint.X;
        if Abs(DeltaX) > 1 then
        begin
          LColWidth := ColWidths[LeftCol];
          LCols := Abs(DeltaX) div LColWidth;
          if (Abs(DeltaX) mod LColWidth = 0) or (LCols > 0) then
          begin
            for I := 0 to LCols - 1 do
              ModifyScrollBar(SB_HORZ, HorizScrollFlags[DeltaX > 0], 0, True);
            FPanPoint := EventInfo.Location;
            Inc(FPanPoint.X, DeltaX mod LColWidth);
          end;
        end;
      end;

    end;
  end;
end;

procedure TCustomGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewTopLeft, NewCurrent, MaxTopLeft: TGridCoord;
  DrawInfo: TGridDrawInfo;
  PageWidth, PageHeight: Integer;
  RTLFactor: Integer;
  NeedsInvalidating: Boolean;

  procedure CalcPageExtents;
  begin
    CalcDrawInfo(DrawInfo);
    PageWidth := DrawInfo.Horz.LastFullVisibleCell - LeftCol;
    if PageWidth < 1 then PageWidth := 1;
    PageHeight := DrawInfo.Vert.LastFullVisibleCell - TopRow;
    if PageHeight < 1 then PageHeight := 1;
  end;

  procedure Restrict(var Coord: TGridCoord; MinX, MinY, MaxX, MaxY: Longint);
  begin
    with Coord do
    begin
      if X > MaxX then X := MaxX
      else if X < MinX then X := MinX;
      if Y > MaxY then Y := MaxY
      else if Y < MinY then Y := MinY;
    end;
  end;

begin
  inherited KeyDown(Key, Shift);
  NeedsInvalidating := False;
  if not CanGridAcceptKey(Key, Shift) then Key := 0;
  if not UseRightToLeftAlignment then
    RTLFactor := 1
  else
    RTLFactor := -1;
  NewCurrent := FCurrent;
  NewTopLeft := FTopLeft;
  CalcPageExtents;
  if ssCtrl in Shift then
    case Key of
      VK_UP: Dec(NewTopLeft.Y);
      VK_DOWN: Inc(NewTopLeft.Y);
      VK_LEFT:
        if not (goRowSelect in Options) then
        begin
          Dec(NewCurrent.X, PageWidth * RTLFactor);
          Dec(NewTopLeft.X, PageWidth * RTLFactor);
        end;
      VK_RIGHT:
        if not (goRowSelect in Options) then
        begin
          Inc(NewCurrent.X, PageWidth * RTLFactor);
          Inc(NewTopLeft.X, PageWidth * RTLFactor);
        end;
      VK_PRIOR: NewCurrent.Y := TopRow;
      VK_NEXT: NewCurrent.Y := DrawInfo.Vert.LastFullVisibleCell;
      VK_HOME:
        begin
          NewCurrent.X := {FixedCols}1;
          NewCurrent.Y := {FixedRows}0;
          NeedsInvalidating := UseRightToLeftAlignment;
        end;
      VK_END:
        begin
          NewCurrent.X := ColCount - 1;
          NewCurrent.Y := RowCount - 1;
          NeedsInvalidating := UseRightToLeftAlignment;
        end;
    end
  else
    case Key of
      VK_UP: Dec(NewCurrent.Y);
      VK_DOWN: Inc(NewCurrent.Y);
      VK_LEFT:
        if goRowSelect in Options then
          Dec(NewCurrent.Y, RTLFactor) else
          Dec(NewCurrent.X, RTLFactor);
      VK_RIGHT:
        if goRowSelect in Options then
          Inc(NewCurrent.Y, RTLFactor) else
          Inc(NewCurrent.X, RTLFactor);
      VK_NEXT:
        begin
          Inc(NewCurrent.Y, PageHeight);
          Inc(NewTopLeft.Y, PageHeight);
        end;
      VK_PRIOR:
        begin
          Dec(NewCurrent.Y, PageHeight);
          Dec(NewTopLeft.Y, PageHeight);
        end;
      VK_HOME:
        if goRowSelect in Options then
          NewCurrent.Y := {FixedRows}0 else
          NewCurrent.X := {FixedCols}1;
      VK_END:
        if goRowSelect in Options then
          NewCurrent.Y := RowCount - 1 else
          NewCurrent.X := ColCount - 1;
      VK_TAB:
        if not (ssAlt in Shift) then
        repeat
          if ssShift in Shift then
          begin
            Dec(NewCurrent.X);
            if NewCurrent.X < {FixedCols}1 then
            begin
              NewCurrent.X := ColCount - 1;
              Dec(NewCurrent.Y);
              if NewCurrent.Y < {FixedRows}0 then NewCurrent.Y := RowCount - 1;
            end;
            Shift := [];
          end
          else
          begin
            Inc(NewCurrent.X);
            if NewCurrent.X >= ColCount then
            begin
              NewCurrent.X := {FixedCols}1;
              Inc(NewCurrent.Y);
              if NewCurrent.Y >= RowCount then NewCurrent.Y := {FixedRows}0;
            end;
          end;
        until TabStops[NewCurrent.X] or (NewCurrent.X = FCurrent.X);
      VK_F2: EditorMode := True;
    end;
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  Restrict(NewTopLeft, {FixedCols}1, {FixedRows}0, MaxTopLeft.X, MaxTopLeft.Y);
  if (NewTopLeft.X <> LeftCol) or (NewTopLeft.Y <> TopRow) then
    MoveTopLeft(NewTopLeft.X, NewTopLeft.Y);
  Restrict(NewCurrent, {FixedCols}1, {FixedRows}0, ColCount - 1, RowCount - 1);
  if (NewCurrent.X <> Col) or (NewCurrent.Y <> Row) then
    FocusCell(NewCurrent.X, NewCurrent.Y, not (ssShift in Shift));
  if NeedsInvalidating then Invalidate;
end;

procedure TCustomGrid.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if not (goAlwaysShowEditor in Options) and (Key = #13) then
  begin
    if FEditorMode then
      HideEditor else
      ShowEditor;
    Key := #0;
  end;
end;

procedure TCustomGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CellHit: TGridCoord;
  DrawInfo: TGridDrawInfo;
  MoveDrawn: Boolean;
begin
  MoveDrawn := False;
  HideEdit;
  if not (csDesigning in ComponentState) and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    SetFocus;
    if not IsActiveControl then
    begin
      MouseCapture := False;
      Exit;
    end;
  end;
  if (Button = mbLeft) and (ssDouble in Shift) then
    DblClick
  else if Button = mbLeft then
  begin
    CalcDrawInfo(DrawInfo);
    { Check grid sizing }
    CalcSizingState(X, Y, FGridState, FSizingIndex, FSizingPos, FSizingOfs,
      DrawInfo);
    if FGridState <> gsNormal then
    begin
      if (FGridState = gsColSizing) and UseRightToLeftAlignment then
        FSizingPos := ClientWidth - FSizingPos;
      Exit;
    end;
    CellHit := CalcCoordFromPoint(X, Y, DrawInfo);
    if (CellHit.X >= {FixedCols}1) and (CellHit.Y >= {FixedRows}0) then
    begin
      if goEditing in Options then
      begin
        if (CellHit.X = FCurrent.X) and (CellHit.Y = FCurrent.Y) then
          ShowEditor
        else
        begin
          MoveCurrent(CellHit.X, CellHit.Y, True, True);
          UpdateEdit;
        end;
        Click;
      end
      else
      begin
        FGridState := gsSelecting;
        SetTimer(Handle, 1, 60, nil);
        if ssShift in Shift then
          MoveAnchor(CellHit)
        else
          MoveCurrent(CellHit.X, CellHit.Y, True, True);
      end;
    end
    else
    begin
      if (FHotTrackCell.Coord.X <> -1) or (FHotTrackCell.Coord.Y <> -1) then
      begin
        FHotTrackCell.Pressed := True;
        FHotTrackCell.Button := Button;
        InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
      end;

      if (goRowMoving in Options) and (CellHit.X >= 0) and
        (CellHit.X < {FixedCols}1) and (CellHit.Y >= {FixedRows}0) then
      begin
        FMoveIndex := CellHit.Y;
        FMovePos := FMoveIndex;
        if BeginRowDrag(FMoveIndex, FMovePos, Point(X,Y)) then
        begin
          FGridState := gsRowMoving;
          Update;
          DrawMove;
          MoveDrawn := True;
          SetTimer(Handle, 1, 60, nil);
        end;
      end
      else if (goColMoving in Options) and (CellHit.Y >= 0) and
        (CellHit.Y < {FixedRows}0) and (CellHit.X >= {FixedCols}1) then
      begin
        FMoveIndex := CellHit.X;
        FMovePos := FMoveIndex;
        if BeginColumnDrag(FMoveIndex, FMovePos, Point(X,Y)) then
        begin
          FGridState := gsColMoving;
          Update;
          DrawMove;
          MoveDrawn := True;
          SetTimer(Handle, 1, 60, nil);
        end;
      end;
    end;
  end;
  try
    inherited MouseDown(Button, Shift, X, Y);
  except
    if MoveDrawn then DrawMove;
  end;
end;

procedure TCustomGrid.MouseMove(Shift: TShiftState; X, Y: Integer);

  function ResizeLine(const AxisInfo: TGridAxisDrawInfo): Integer;
  var
    I: Integer;
  begin
    with AxisInfo do
    begin
      Result := FixedBoundary;
      for I := FirstGridCell to FSizingIndex - 1 do
        Inc(Result, AxisInfo.GetExtent(I) + EffectiveLineWidth);
      Result := FSizingPos - Result;
    end;
  end;

var
  DrawInfo: TGridDrawInfo;
  CellHit: TGridCoord;
  NewSize: Integer;
begin
  CalcDrawInfo(DrawInfo);
  case FGridState of
    gsColMoving, gsRowMoving:
      begin
        CellHit := CalcCoordFromPoint(X, Y, DrawInfo);
        if (CellHit.X >= {FixedCols}1) and (CellHit.Y >= {FixedRows}0) and
          (CellHit.X <= DrawInfo.Horz.LastFullVisibleCell+1) and
          (CellHit.Y <= DrawInfo.Vert.LastFullVisibleCell+1) then
          case FGridState of
            gsColMoving:
              MoveAndScroll(X, CellHit.X, DrawInfo, DrawInfo.Horz, SB_HORZ, Point(X,Y));
            gsRowMoving:
              MoveAndScroll(Y, CellHit.Y, DrawInfo, DrawInfo.Vert, SB_VERT, Point(X,Y));
          end;
      end;
    gsColSizing:
      begin
        FSizingPos := X + FSizingOfs;
        CalcDrawInfo(DrawInfo);
        if (FGridState = gsColSizing) and UseRightToLeftAlignment then
          FSizingPos := ClientWidth - FSizingPos;
        if FGridState = gsColSizing then
        begin
          NewSize := ResizeLine(DrawInfo.Horz);
          if NewSize > 1 then
          begin
            ColWidths[FSizingIndex] := NewSize;
            UpdateDesigner;
          end;
        end
        else
        begin
          NewSize := ResizeLine(DrawInfo.Vert);
          if NewSize > 1 then
          begin
            RowHeights[FSizingIndex] := NewSize;
            UpdateDesigner;
          end;
        end;
      end;
    else
    begin
      if (csDesigning in ComponentState) then
        Exit;
      // Highlight "fixed" cell
      CellHit := CalcCoordFromPoint(X, Y, DrawInfo);
      if ((goFixedRowClick in FOptions) and (CellHit.Y <= {FixedRows}0)) or
         ((goFixedColClick in FOptions) and (CellHit.X <= {FixedCols}1)) then
      begin
        if (FHotTrackCell.Coord.X <> -1) or (FHotTrackCell.Coord.Y <> -1) then
          InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
        if (CellHit.X <> FHotTrackCell.Coord.X) or (CellHit.Y <> FHotTrackCell.Coord.Y) then
        begin
          FHotTrackCell.Coord := CellHit;
          FHotTrackCell.Pressed := False;
          InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
        end;
      end
      else
        if (FHotTrackCell.Coord.X <> -1) or (FHotTrackCell.Coord.Y <> -1) then
        begin
          InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
          FHotTrackCell.Coord.X := -1;
          FHotTrackCell.Coord.Y := -1;
          FHotTrackCell.Pressed := False;
        end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DrawInfo: TGridDrawInfo;
  NewSize: Integer;
  Cell: TGridCoord;

  function ResizeLine(const AxisInfo: TGridAxisDrawInfo): Integer;
  var
    I: Integer;
  begin
    with AxisInfo do
    begin
      Result := FixedBoundary;
      for I := FirstGridCell to FSizingIndex - 1 do
        Inc(Result, AxisInfo.GetExtent(I) + EffectiveLineWidth);
      Result := FSizingPos - Result;
    end;
  end;

begin
  try
    case FGridState of
      gsSelecting:
        begin
          MouseMove(Shift, X, Y);
          KillTimer(Handle, 1);
          UpdateEdit;
          Click;
        end;
      gsColMoving:
        begin
          DrawMove;
          KillTimer(Handle, 1);
          if EndColumnDrag(FMoveIndex, FMovePos, Point(X,Y))
            and (FMoveIndex <> FMovePos) then
          begin
            MoveColumn(FMoveIndex, FMovePos);
            UpdateDesigner;
          end;
          UpdateEdit;
        end;
      gsRowMoving:
        begin
          DrawMove;
          KillTimer(Handle, 1);
          if EndRowDrag(FMoveIndex, FMovePos, Point(X,Y))
            and (FMoveIndex <> FMovePos) then
          begin
            MoveRow(FMoveIndex, FMovePos);
            UpdateDesigner;
          end;
          UpdateEdit;
        end;
    else
      UpdateEdit;
      Cell := MouseCoord(X, Y);
      if (Button = mbLeft) and (FHotTrackCell.Coord.X <> -1) and (FHotTrackCell.Coord.Y <> -1) and
         (((goFixedColClick in FOptions) and (Cell.X < {FFixedCols}1) and (Cell.X >= 0)) or
         ((goFixedRowClick in FOptions) and (Cell.Y < {FFixedRows}0) and (Cell.Y >= 0))) then
        FixedCellClick(Cell.X, Cell.Y);
    end;
    inherited MouseUp(Button, Shift, X, Y);
  finally
    FGridState := gsNormal;
    FHotTrackCell.Pressed := False;
    InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
  end;
end;

procedure TCustomGrid.MoveAndScroll(Mouse, CellHit: Integer;
  var DrawInfo: TGridDrawInfo; var Axis: TGridAxisDrawInfo;
  ScrollBar: Integer; const MousePt: TPoint);
begin
  if UseRightToLeftAlignment and (ScrollBar = SB_HORZ) then
    Mouse := ClientWidth - Mouse;
  if (CellHit <> FMovePos) and
    not((FMovePos = Axis.FixedCellCount) and (Mouse < Axis.FixedBoundary)) and
    not((FMovePos = Axis.GridCellCount-1) and (Mouse > Axis.GridBoundary)) then
  begin
    DrawMove;   // hide the drag line
    if (Mouse < Axis.FixedBoundary) then
    begin
      if (FMovePos > Axis.FixedCellCount) then
      begin
        ModifyScrollbar(ScrollBar, SB_LINEUP, 0, False);
        Update;
        CalcDrawInfo(DrawInfo);    // this changes contents of Axis var
      end;
      CellHit := Axis.FirstGridCell;
    end
    else if (Mouse >= Axis.FullVisBoundary) then
    begin
      if (FMovePos = Axis.LastFullVisibleCell) and
        (FMovePos < Axis.GridCellCount -1) then
      begin
        ModifyScrollBar(Scrollbar, SB_LINEDOWN, 0, False);
        Update;
        CalcDrawInfo(DrawInfo);    // this changes contents of Axis var
      end;
      CellHit := Axis.LastFullVisibleCell;
    end
    else if CellHit < 0 then CellHit := FMovePos;
    if ((FGridState = gsColMoving) and CheckColumnDrag(FMoveIndex, CellHit, MousePt))
      or ((FGridState = gsRowMoving) and CheckRowDrag(FMoveIndex, CellHit, MousePt)) then
      FMovePos := CellHit;
    DrawMove;
  end;
end;

function TCustomGrid.GetColWidths(Index: Longint): Integer;
begin
  if (FColWidths = nil) or (Index >= ColCount) then
    Result := DefaultColWidth
  else
    Result := PIntArray(FColWidths)^[Index + 1];
end;

function TCustomGrid.GetRowHeights(Index: Longint): Integer;
begin
  if (FRowHeights = nil) or (Index >= RowCount) then
    Result := DefaultRowHeight
  else
    Result := PIntArray(FRowHeights)^[Index + 1];
end;

function TCustomGrid.GetGridWidth: Integer;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Horz.GridBoundary;
end;

function TCustomGrid.GetGridHeight: Integer;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Vert.GridBoundary;
end;

function TCustomGrid.GetSelection: TGridRect;
begin
  Result := GridRect(FCurrent, FAnchor);
end;

function TCustomGrid.GetTabStops(Index: Longint): Boolean;
begin
  if FTabStops = nil then Result := True
  else Result := Boolean(PIntArray(FTabStops)^[Index + 1]);
end;

function TCustomGrid.GetVisibleColCount: Integer;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Horz.LastFullVisibleCell - LeftCol + 1;
end;

function TCustomGrid.GetVisibleRowCount: Integer;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Vert.LastFullVisibleCell - TopRow + 1;
end;

procedure TCustomGrid.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGrid.SetChangedValueColor(Value: TColor);
begin
  if FChangedValueColor <> Value then
  begin
    FChangedValueColor := Value;
    Invalidate;
  end;
end;

procedure TCustomGrid.SetCol(Value: Longint);
begin
  if Col <> Value then FocusCell(Value, Row, True);
end;

procedure TCustomGrid.SetColCount(Value: Longint);
begin
  if FColCount <> Value then
  begin
    if Value < 1 then Value := 1;
//    if Value <= FixedCols then FixedCols := Value - 1;
    ChangeSize(Value, RowCount);
    if goRowSelect in Options then
    begin
      FAnchor.X := ColCount - 1;
      Invalidate;
    end;
  end;
end;

procedure TCustomGrid.SetColWidths(Index: Longint; Value: Integer);
begin
  if FColWidths = nil then
    UpdateExtents(FColWidths, ColCount, DefaultColWidth);
  if Index >= ColCount then InvalidOp(SIndexOutOfRange);
  if Value <> PIntArray(FColWidths)^[Index + 1] then
  begin
    ResizeCol(Index, PIntArray(FColWidths)^[Index + 1], Value);
    PIntArray(FColWidths)^[Index + 1] := Value;
    ColWidthsChanged;
  end;
end;

procedure TCustomGrid.SetDefaultColWidth(Value: Integer);
begin
  if FColWidths <> nil then
    UpdateExtents(FColWidths, 0, 0);
  FDefaultColWidth := Value;
  ColWidthsChanged;
  InvalidateGrid;
end;

procedure TCustomGrid.SetDefaultRowHeight(Value: Integer);
begin
  if FRowHeights <> nil then
    UpdateExtents(FRowHeights, 0, 0);
  FDefaultRowHeight := Value;
  RowHeightsChanged;
  InvalidateGrid;
end;

//procedure TCustomGrid.SetDrawingStyle(const Value: TGridDrawingStyle);
//begin
//  if Value <> FDrawingStyle then
//  begin
//    FDrawingStyle := Value;
//    FInternalDrawingStyle := FDrawingStyle;
//    if (FDrawingStyle = gdsThemed) {and not ThemeControl(Self)} then
//      FInternalDrawingStyle := gdsClassic;
//    Repaint;
//  end;
//end;

procedure TCustomGrid.SetEditorMode(Value: Boolean);
begin
  if not Value then
    HideEditor
  else
  begin
    ShowEditor;
//    if FInplaceEdit <> nil then FInplaceEdit.Deselect;
  end;
end;

procedure TCustomGrid.SetGradientEndColor(Value: TColor);
begin
  if Value <> FGradientEndColor then
  begin
    FGradientEndColor := Value;
    if HandleAllocated then
      Repaint;
  end;
end;

procedure TCustomGrid.SetGradientStartColor(Value: TColor);
begin
  if Value <> FGradientStartColor then
  begin
    FGradientStartColor := Value;
    if HandleAllocated then
      Repaint;
  end;
end;

procedure TCustomGrid.SetGridLineWidth(Value: Integer);
begin
  if FGridLineWidth <> Value then
  begin
    FGridLineWidth := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGrid.SetHighlightingColor(Value: TColor);
begin
  if FHighlightingColor <> Value then
  begin
    FHighlightingColor := Value;
    Invalidate;
  end;
end;

procedure TCustomGrid.SetLeftCol(Value: Longint);
begin
  if FTopLeft.X <> Value then MoveTopLeft(Value, TopRow);
end;

procedure TCustomGrid.SetLinesColor(Value: TColor);
begin
  if FLinesColor <> Value then
  begin
    FLinesColor := Value;
    Invalidate;
  end;
end;

procedure TCustomGrid.SetOptions(Value: TGridOptions);
begin
  if FOptions <> Value then
  begin
    if goRowSelect in Value then
      Exclude(Value, goAlwaysShowEditor);
    FOptions := Value;
    if not FEditorMode then
      if goAlwaysShowEditor in Value then
        ShowEditor else
        HideEditor;
    if goRowSelect in Value then MoveCurrent(Col, Row,  True, False);
    InvalidateGrid;
  end;
end;

procedure TCustomGrid.SetRow(Value: Longint);
begin
  if Row <> Value then FocusCell(Col, Value, True);
end;

procedure TCustomGrid.SetRowCount(Value: Longint);
begin
  if FRowCount <> Value then
  begin
    if Value < 1 then Value := 1;
    ChangeSize(ColCount, Value);
  end;
end;

procedure TCustomGrid.SetRowHeights(Index: Longint; Value: Integer);
begin
  if FRowHeights = nil then
    UpdateExtents(FRowHeights, RowCount, DefaultRowHeight);
  if Index >= RowCount then InvalidOp(SIndexOutOfRange);
  if Value <> PIntArray(FRowHeights)^[Index + 1] then
  begin
    ResizeRow(Index, PIntArray(FRowHeights)^[Index + 1], Value);
    PIntArray(FRowHeights)^[Index + 1] := Value;
    RowHeightsChanged;
  end;
end;

procedure TCustomGrid.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGrid.SetSelection(Value: TGridRect);
var
  OldSel: TGridRect;
begin
  OldSel := Selection;
  FAnchor.X := Value.Left;
  FAnchor.Y := Value.Top;
  FCurrent.X := Value.Right;
  FCurrent.Y := Value.Bottom;
  SelectionMoved(OldSel);
end;

procedure TCustomGrid.SetTabStops(Index: Longint; Value: Boolean);
begin
  if FTabStops = nil then
    UpdateExtents(FTabStops, ColCount, Integer(True));
  if Index >= ColCount then InvalidOp(SIndexOutOfRange);
  PIntArray(FTabStops)^[Index + 1] := Integer(Value);
end;

procedure TCustomGrid.SetTopRow(Value: Longint);
begin
  if FTopLeft.Y <> Value then MoveTopLeft(LeftCol, Value);
end;

procedure TCustomGrid.HideEdit;
begin
//  if FInplaceEdit <> nil then
//    try
//      UpdateText;
//    finally
//      FInplaceCol := -1;
//      FInplaceRow := -1;
//      FInplaceEdit.Hide;
//    end;
end;

procedure TCustomGrid.UpdateEdit;

//  procedure UpdateEditor;
//  begin
//    FInplaceCol := Col;
//    FInplaceRow := Row;
//    FInplaceEdit.UpdateContents;
//    if FInplaceEdit.MaxLength = -1 then FCanEditModify := False
//    else FCanEditModify := True;
//    FInplaceEdit.SelectAll;
//  end;

begin
//  if CanEditShow then
//  begin
//    if FInplaceEdit = nil then
//    begin
//      FInplaceEdit := CreateEditor;
//      FInplaceEdit.SetGrid(Self);
//      FInplaceEdit.Parent := Self;
//      UpdateEditor;
//    end
//    else
//    begin
//      if (Col <> FInplaceCol) or (Row <> FInplaceRow) then
//      begin
//        HideEdit;
//        UpdateEditor;
//      end;
//    end;
//    if CanEditShow then FInplaceEdit.Move(CellRect(Col, Row));
//  end;
end;

procedure TCustomGrid.UpdateText;
begin
//  if (FInplaceCol <> -1) and (FInplaceRow <> -1) then
//    SetEditText(FInplaceCol, FInplaceRow, FInplaceEdit.Text);
end;

procedure TCustomGrid.WMChar(var Msg: TWMChar);
begin
  if (goEditing in Options) and (CharInSet(Char(Msg.CharCode), [^H]) or
     (Char(Msg.CharCode) >= #32)) then
    ShowEditorChar(Char(Msg.CharCode))
  else
    inherited;
end;

procedure TCustomGrid.WMCommand(var Message: TWMCommand);
begin
//  with Message do
//  begin
//    if (FInplaceEdit <> nil) and (Ctl = FInplaceEdit.Handle) then
//      case NotifyCode of
//        EN_CHANGE: UpdateText;
//      end;
//  end;
end;

procedure TCustomGrid.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
  if goRowSelect in Options then Exit;
  if goTabs in Options then Msg.Result := Msg.Result or DLGC_WANTTAB;
  if goEditing in Options then Msg.Result := Msg.Result or DLGC_WANTCHARS;
end;

procedure TCustomGrid.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  DestroyCaret;
  InvalidateRect(Selection);
//  if (FInplaceEdit <> nil) and (Msg.FocusedWnd <> FInplaceEdit.Handle) then
//    HideEdit;
end;

procedure TCustomGrid.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
//  if FInplaceEdit <> nil then FInplaceEdit.FClickTime := GetMessageTime;
end;

procedure TCustomGrid.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
  FHitTest := ScreenToClient(SmallPointToPoint(Msg.Pos));
end;

procedure TCustomGrid.WMSetCursor(var Msg: TWMSetCursor);
var
  DrawInfo: TGridDrawInfo;
  State: TGridState;
  Index: Longint;
  Pos, Ofs: Integer;
  Cur: HCURSOR;
begin
  Cur := 0;
  with Msg do
  begin
    if HitTest = HTCLIENT then
    begin
      if FGridState = gsNormal then
      begin
        CalcDrawInfo(DrawInfo);
        CalcSizingState(FHitTest.X, FHitTest.Y, State, Index, Pos, Ofs,
          DrawInfo);
      end else State := FGridState;
      if State = gsColSizing then
        Cur := Screen.Cursors[crSizeWE]
    end;
  end;
  if Cur <> 0 then SetCursor(Cur)
  else inherited;
end;

procedure TCustomGrid.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  CreateCaret(Handle, 0, 0, 0);
//  if (FInplaceEdit = nil) or (Msg.FocusedWnd <> FInplaceEdit.Handle) then
//  begin
//    InvalidateRect(Selection);
//    UpdateEdit;
//  end;
end;

procedure TCustomGrid.WMSize(var Msg: TWMSize);
begin
  inherited;
  UpdateScrollRange;
  if UseRightToLeftAlignment then Invalidate;
end;

procedure TCustomGrid.WMVScroll(var Msg: TWMVScroll);
begin
  ModifyScrollBar(SB_VERT, Msg.ScrollCode, Msg.Pos, True);
end;

procedure TCustomGrid.WMHScroll(var Msg: TWMHScroll);
begin
  ModifyScrollBar(SB_HORZ, Msg.ScrollCode, Msg.Pos, True);
end;

procedure TCustomGrid.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
  Size: TSize;
begin
  { Fill the area between the two scroll bars. }
  Size.cx := GetSystemMetrics(SM_CXVSCROLL);
  Size.cy := GetSystemMetrics(SM_CYHSCROLL);
  if UseRightToLeftAlignment then
    R := Bounds(0, Height - Size.cy, Size.cx, Size.cy)
  else
    R := Bounds(Width - Size.cx, Height - Size.cy, Size.cx, Size.cy);
  FillRect(Message.DC, R, Brush.Handle);
  Message.Result := 1;
end;

procedure TCustomGrid.CancelMode;
var
  DrawInfo: TGridDrawInfo;
begin
  try
    case FGridState of
      gsSelecting:
        KillTimer(Handle, 1);
      gsColSizing:
          CalcDrawInfo(DrawInfo);
      gsColMoving, gsRowMoving:
        begin
          DrawMove;
          KillTimer(Handle, 1);
        end;
    end;
  finally
    FGridState := gsNormal;
  end;
end;

procedure TCustomGrid.WMCancelMode(var Msg: TWMCancelMode);
begin
  inherited;
  CancelMode;
end;

procedure TCustomGrid.CMCancelMode(var Msg: TCMCancelMode);
begin
//  if Assigned(FInplaceEdit) then
//    FInplaceEdit.WndProc(TMessage(Msg));
  inherited;
  CancelMode;
end;

procedure TCustomGrid.CMFontChanged(var Message: TMessage);
begin
//  if FInplaceEdit <> nil then FInplaceEdit.Font := Font;
  inherited;
end;

procedure TCustomGrid.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHotTrackCell.Coord.X <> -1) or (FHotTrackCell.Coord.Y <> -1) then
  begin
    InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
    FHotTrackCell.Coord.X := -1;
    FHotTrackCell.Coord.Y := -1;
  end;
end;

procedure TCustomGrid.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure TCustomGrid.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  Msg.Result := Longint(BOOL(Sizing(Msg.Pos.X, Msg.Pos.Y)));
end;

procedure TCustomGrid.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if (goEditing in Options) and (Char(Msg.CharCode) = #13) then Msg.Result := 1;
end;

procedure TCustomGrid.TimedScroll(Direction: TGridScrollDirection);
var
  MaxAnchor, NewAnchor: TGridCoord;
begin
  NewAnchor := FAnchor;
  MaxAnchor.X := ColCount - 1;
  MaxAnchor.Y := RowCount - 1;
  if (sdLeft in Direction) and (FAnchor.X > {FixedCols}1) then Dec(NewAnchor.X);
  if (sdRight in Direction) and (FAnchor.X < MaxAnchor.X) then Inc(NewAnchor.X);
  if (sdUp in Direction) and (FAnchor.Y > {FixedRows}0) then Dec(NewAnchor.Y);
  if (sdDown in Direction) and (FAnchor.Y < MaxAnchor.Y) then Inc(NewAnchor.Y);
  if (FAnchor.X <> NewAnchor.X) or (FAnchor.Y <> NewAnchor.Y) then
    MoveAnchor(NewAnchor);
end;

procedure TCustomGrid.WMTimer(var Msg: TWMTimer);
var
  Point: TPoint;
  DrawInfo: TGridDrawInfo;
  ScrollDirection: TGridScrollDirection;
  CellHit: TGridCoord;
  LeftSide: Integer;
  RightSide: Integer;
begin
  if not (FGridState in [gsRowMoving, gsColMoving]) then Exit;
  GetCursorPos(Point);
  Point := ScreenToClient(Point);
  CalcDrawInfo(DrawInfo);
  ScrollDirection := [];
  with DrawInfo do
  begin
    CellHit := CalcCoordFromPoint(Point.X, Point.Y, DrawInfo);
    case FGridState of
      gsColMoving:
        MoveAndScroll(Point.X, CellHit.X, DrawInfo, Horz, SB_HORZ, Point);
      gsRowMoving:
        MoveAndScroll(Point.Y, CellHit.Y, DrawInfo, Vert, SB_VERT, Point);
    end;
  end;
end;

procedure TCustomGrid.ColWidthsChanged;
begin
  UpdateScrollRange;
  UpdateEdit;
end;

procedure TCustomGrid.RowHeightsChanged;
begin
  UpdateScrollRange;
  UpdateEdit;
end;

procedure TCustomGrid.DeleteColumn(ACol: Longint);
begin
  MoveColumn(ACol, ColCount-1);
  ColCount := ColCount - 1;
end;

procedure TCustomGrid.DeleteRow(ARow: Longint);
begin
  MoveRow(ARow, RowCount - 1);
  RowCount := RowCount - 1;
end;

procedure TCustomGrid.UpdateDesigner;
var
  ParentForm: TCustomForm;
begin
  if (csDesigning in ComponentState) and HandleAllocated and
    not (csUpdating in ComponentState) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
      ParentForm.Designer.Modified;
  end;
end;

function TCustomGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    if Row < RowCount - 1 then Row := Row + 1;
    Result := True;
  end;
end;

function TCustomGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    if Row > {FixedRows}0 then Row := Row - 1;
    Result := True;
  end;
end;

function TCustomGrid.CheckColumnDrag(var Origin,
  Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TCustomGrid.CheckRowDrag(var Origin,
  Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TCustomGrid.BeginColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TCustomGrid.BeginRowDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TCustomGrid.EndColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TCustomGrid.EndRowDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

procedure TCustomGrid.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then UpdateScrollRange;
end;

{ TCustomDrawGrid }

function TCustomDrawGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := inherited CellRect(ACol, ARow);
end;

procedure TCustomDrawGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

procedure TCustomDrawGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;

function TCustomDrawGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
  if Assigned(FOnGetEditMask) then FOnGetEditMask(Self, ACol, ARow, Result);
end;

function TCustomDrawGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := '';
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TCustomDrawGrid.RowMoved(FromIndex, ToIndex: Longint);
begin
  if Assigned(FOnRowMoved) then FOnRowMoved(Self, FromIndex, ToIndex);
end;

function TCustomDrawGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
  if Assigned(FOnSelectCell) then FOnSelectCell(Self, ACol, ARow, Result);
end;

procedure TCustomDrawGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  if Assigned(FOnSetEditText) then FOnSetEditText(Self, ACol, ARow, Value);
end;

procedure TCustomDrawGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  Hold: Integer;
begin
  if Assigned(FOnDrawCell) then
  begin
    if UseRightToLeftAlignment then
    begin
      ARect.Left := ClientWidth - ARect.Left;
      ARect.Right := ClientWidth - ARect.Right;
      Hold := ARect.Left;
      ARect.Left := ARect.Right;
      ARect.Right := Hold;
      ChangeGridOrientation(False);
    end;
    FOnDrawCell(Self, ACol, ARow, ARect, AState);
    if UseRightToLeftAlignment then ChangeGridOrientation(True);
  end;
end;

procedure TCustomDrawGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;

{ StrItem management for TStringSparseList }

type

  PStrItem = ^TStrItem;
  TStrItem = record
    FObject: TObject;
    FString: string;
  end;

  TStrItemType = PStrItem;

function NewStrItem(const AString: string; AObject: TObject): TStrItemType;
begin
  New(Result);
  Result.FObject := AObject;
  Result.FString := AString;
end;

procedure DisposeStrItem(P: PStrItem);
begin
  Dispose(P);
end;

{ Sparse array classes for TStringGrid }

type
{ Exception classes }

  EStringSparseListError = class(Exception);

{ TSparsePointerArray class}

{ Used by TSparseList.  Based on Sparse1Array, but has Pointer elements
  and Integer index, just like TPointerList/TList, and less indirection }

  { Apply function for the applicator:
        TheIndex        Index of item in array
        TheItem         Value of item (i.e pointer element) in section
        Returns: 0 if success, else error code. }
  TSPAApply = function(TheIndex: Integer; TheItem: Pointer): Integer;

  TSecDir = array[0..4095] of Pointer;  { Enough for up to 12 bits of sec }
  PSecDir = ^TSecDir;
  TSecDirType = PSecDir;
  TSPAQuantum = (SPASmall, SPALarge);   { Section size }

  TSparsePointerArray = class(TObject)
  private
    secDir: TSecDirType;
    slotsInDir: Word;
    indexMask, secShift: Word;
    FHighBound: Integer;
    FSectionSize: Word;
    cachedIndex: Integer;
    cachedValue: TCustomData;
    { Return item[i], nil if slot outside defined section. }
    function GetAt(Index: Integer): TCustomData;
    { Store item at item[i], creating slot if necessary. }
    procedure PutAt(Index: Integer; Item: TCustomData);
    { Return address of item[i], creating slot if necessary. }
    function MakeAt(Index: Integer): PPointer;
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor Destroy; override;

    { Traverse SPA, calling apply function for each defined non-nil
      item.  The traversal terminates if the apply function returns
      a value other than 0. }

    // WIN32: Must be static method so that we can take its address in TSparseList.ForAll
    function  ForAll(ApplyFunction: Pointer {TSPAApply}): Integer;

    { Ratchet down HighBound after a deletion }
    procedure ResetHighBound;

    property HighBound: Integer read FHighBound;
    property SectionSize: Word read FSectionSize;
    property Items[Index: Integer]: TCustomData read GetAt write PutAt; default;
  end;

{ TSparseList class }

  TSparseList = class(TObject)
  private
    FList: TSparsePointerArray;
    FCount: Integer;    { 1 + HighBound, adjusted for Insert/Delete }
    FQuantum: TSPAQuantum;
    procedure NewList(Quantum: TSPAQuantum);
  protected
    function  Get(Index: Integer): TCustomData;
    procedure Put(Index: Integer; Item: TCustomData);
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; Item: TCustomData);
    procedure Move(CurIndex, NewIndex: Integer);
    function ForAll(ApplyFunction: Pointer {TSPAApply}): Integer;
    property Count: Integer read FCount;
    property Items[Index: Integer]: TCustomData read Get write Put; default;
  end;

{ TStringSparseList class }

  TStringSparseList = class(TStrings)
  private
    FList: TSparseList;                 { of StrItems }
    FOnChange: TNotifyEvent;
  protected
    // TStrings overrides
    function  Get(Index: Integer): String; override;
    function  GetCount: Integer; override;
    function  GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure Changed;
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure Clear; override;
    property List: TSparseList read FList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TSparsePointerArray }

const
  SPAIndexMask: array[TSPAQuantum] of Byte = (15, 255);
  SPASecShift: array[TSPAQuantum] of Byte = (4, 8);

{ Expand Section Directory to cover at least `newSlots' slots. Returns: Possibly
  updated pointer to the Section Directory. }
function  ExpandDir(secDir: TSecDirType; var slotsInDir: Word;
  newSlots: Word): TSecDirType;
begin
  Result := secDir;
  ReallocMem(Result, newSlots * SizeOf(Pointer));
  FillChar(Result^[slotsInDir], (newSlots - slotsInDir) * SizeOf(Pointer), 0);
  slotsInDir := newSlots;
end;

{ Allocate a section and set all its items to nil. Returns: Pointer to start of
  section. }
function  MakeSec(SecIndex: Integer; SectionSize: Word): Pointer;
var
  SecP: Pointer;
  Size: Word;
begin
  Size := SectionSize * SizeOf(Pointer);
  GetMem(secP, size);
  FillChar(secP^, size, 0);
  MakeSec := SecP
end;

constructor TSparsePointerArray.Create(Quantum: TSPAQuantum);
begin
  SecDir := nil;
  SlotsInDir := 0;
  FHighBound := -1;
  FSectionSize := Word(SPAIndexMask[Quantum]) + 1;
  IndexMask := Word(SPAIndexMask[Quantum]);
  SecShift := Word(SPASecShift[Quantum]);
  CachedIndex := -1
end;

destructor TSparsePointerArray.Destroy;
var
  i:  Integer;
  size: Word;
begin
  { Scan section directory and free each section that exists. }
  i := 0;
  size := FSectionSize * SizeOf(Pointer);
  while i < slotsInDir do begin
    if secDir^[i] <> nil then
      FreeMem(secDir^[i], size);
    Inc(i)
  end;

  { Free section directory. }
  if secDir <> nil then
    FreeMem(secDir, slotsInDir * SizeOf(Pointer));
end;

function  TSparsePointerArray.GetAt(Index: Integer): TCustomData;
var
  byteP: PByte;
  secIndex: Cardinal;
begin
  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If not null, index into
    Section using low order part of index. }
  if Index = cachedIndex then
    Result := cachedValue
  else begin
    secIndex := Index shr secShift;
    if secIndex >= slotsInDir then
      byteP := nil
    else begin
      byteP := secDir^[secIndex];
      if byteP <> nil then begin
        Inc(byteP, (Index and indexMask) * SizeOf(Pointer));
      end
    end;
    if byteP = nil then Result := nil else Result := PPointer(byteP)^;
    cachedIndex := Index;
    cachedValue := Result
  end
end;

function  TSparsePointerArray.MakeAt(Index: Integer): PPointer;
var
  dirP: PSecDir;
  p: Pointer;
  byteP: PByte;
  secIndex: Word;
begin
  { Expand Section Directory if necessary. }
  secIndex := Index shr secShift;       { Unsigned shift }
  if secIndex >= slotsInDir then
    dirP := expandDir(secDir, slotsInDir, secIndex + 1)
  else
    dirP := secDir;

  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If null, create new
    Section.  Index into Section using low order part of index. }
  secDir := dirP;
  p := dirP^[secIndex];
  if p = nil then begin
    p := makeSec(secIndex, FSectionSize);
    dirP^[secIndex] := p
  end;
  byteP := p;
  Inc(byteP, (Index and indexMask) * SizeOf(Pointer));
  if Index > FHighBound then
    FHighBound := Index;
  Result := PPointer(byteP);
  cachedIndex := -1
end;

procedure TSparsePointerArray.PutAt(Index: Integer; Item: TCustomData);
begin
  if (Item <> nil) or (GetAt(Index) <> nil) then
  begin
    MakeAt(Index)^ := Item;
    if Item = nil then
      ResetHighBound
  end
end;

function  TSparsePointerArray.ForAll(ApplyFunction: Pointer {TSPAApply}):
  Integer;
var
  itemP: PByte;                         { Pointer to item in section }
  item: Pointer;
  i, callerBP: Cardinal;
  j, index: Integer;
begin
  { Scan section directory and scan each section that exists,
    calling the apply function for each non-nil item.
    The apply function must be a far local function in the scope of
    the procedure P calling ForAll.  The trick of setting up the stack
    frame (taken from TurboVision's TCollection.ForEach) allows the
    apply function access to P's arguments and local variables and,
    if P is a method, the instance variables and methods of P's class }
  Result := 0;
  i := 0;
  asm
    mov   eax,[ebp]                     { Set up stack frame for local }
    mov   callerBP,eax
  end;
  while (i < slotsInDir) and (Result = 0) do begin
    itemP := secDir^[i];
    if itemP <> nil then begin
      j := 0;
      index := i shl SecShift;
      while (j < FSectionSize) and (Result = 0) do begin
        item := PPointer(itemP)^;
        if item <> nil then
          { ret := ApplyFunction(index, item.Ptr); }
          asm
            mov   eax,index
            mov   edx,item
            push  callerBP
            call  ApplyFunction
            pop   ecx
            mov   @Result,eax
          end;
        Inc(itemP, SizeOf(Pointer));
        Inc(j);
        Inc(index)
      end
    end;
    Inc(i)
  end;
end;

procedure TSparsePointerArray.ResetHighBound;
var
  NewHighBound: Integer;

  function Detector(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    if TheIndex > FHighBound then
      Result := 1
    else
    begin
      Result := 0;
      if TheItem <> nil then NewHighBound := TheIndex
    end
  end;

begin
  NewHighBound := -1;
  ForAll(@Detector);
  FHighBound := NewHighBound
end;

{ TSparseList }

constructor TSparseList.Create(Quantum: TSPAQuantum);
begin
  inherited Create;
  NewList(Quantum)
end;

destructor TSparseList.Destroy;
begin
  if FList <> nil then FList.Destroy
end;

procedure TSparseList.Clear;
begin
  FList.Destroy;
  NewList(FQuantum);
  FCount := 0
end;

procedure TSparseList.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then Exit;
  for I := Index to FCount - 1 do
    FList[I] := FList[I + 1];
  FList[FCount] := nil;
  Dec(FCount);
end;

procedure TSparseList.Exchange(Index1, Index2: Integer);
var
  Temp: TCustomData;
begin
  Temp := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, Temp);
end;

{ Jump to TSparsePointerArray.ForAll so that it looks like it was called
  from our caller, so that the BP trick works. }

function TSparseList.ForAll(ApplyFunction: Pointer {TSPAApply}): Integer; assembler;
asm
        MOV     EAX,[EAX].TSparseList.FList
        JMP     TSparsePointerArray.ForAll
end;

function TSparseList.Get(Index: Integer): TCustomData;
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  Result := FList[Index]
end;

procedure TSparseList.Insert(Index: Integer; Item: TCustomData);
var
  i: Integer;
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  I := FCount;
  while I > Index do
  begin
    FList[i] := FList[i - 1];
    Dec(i)
  end;
  FList[Index] := Item;
  if Index > FCount then FCount := Index;
  Inc(FCount)
end;

procedure TSparseList.Move(CurIndex, NewIndex: Integer);
var
  Item: TCustomData;
begin
  if CurIndex <> NewIndex then
  begin
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

procedure TSparseList.NewList(Quantum: TSPAQuantum);
begin
  FQuantum := Quantum;
  FList := TSparsePointerArray.Create(Quantum)
end;

procedure TSparseList.Put(Index: Integer; Item: TCustomData);
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  FList[Index] := Item;
  FCount := FList.HighBound + 1
end;

{ TStringSparseList }

constructor TStringSparseList.Create(Quantum: TSPAQuantum);
begin
  inherited Create;
  FList := TSparseList.Create(Quantum)
end;

destructor  TStringSparseList.Destroy;
begin
  if FList <> nil then begin
    Clear;
    FList.Destroy
  end
end;

procedure TStringSparseList.ReadData(Reader: TReader);
var
  i: Integer;
begin
  with Reader do begin
    i := Integer(ReadInteger);
    while i > 0 do begin
      InsertObject(Integer(ReadInteger), ReadString, nil);
      Dec(i)
    end
  end
end;


procedure TStringSparseList.WriteData(Writer: TWriter);
var
  itemCount: Integer;

  function  CountItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    Inc(itemCount);
    Result := 0
  end;

  function  StoreItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    with Writer do
    begin
      WriteInteger(TheIndex);           { Item index }
      WriteString(PStrItem(TheItem)^.FString);
    end;
    Result := 0
  end;

begin
  with Writer do
  begin
    itemCount := 0;
    FList.ForAll(@CountItem);
    WriteInteger(itemCount);
    FList.ForAll(@StoreItem);
  end
end;

procedure TStringSparseList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('List', ReadData, WriteData, True);
end;

function  TStringSparseList.Get(Index: Integer): String;
var
  p: TStrItemType;
begin
  p := TStrItemType(FList[Index]);
  if p = nil then Result := '' else Result := p.FString
end;

function  TStringSparseList.GetCount: Integer;
begin
  Result := FList.Count
end;

function  TStringSparseList.GetObject(Index: Integer): TObject;
var
  p: TStrItemType;
begin
  p := TStrItemType(FList[Index]);
  if p = nil then Result := nil else Result := p.FObject
end;

procedure TStringSparseList.Put(Index: Integer; const S: String);
var
  p: TStrItemType;
  obj: TObject;
begin
  p := TStrItemType(FList[Index]);
  if p = nil then obj := nil else obj := p.FObject;
  if (S = '') and (obj = nil) then   { Nothing left to store }
    FList[Index] := nil
  else
    FList[Index] := NewStrItem(S, obj);
  if p <> nil then DisposeStrItem(p);
  Changed
end;

procedure TStringSparseList.PutObject(Index: Integer; AObject: TObject);
var
  p: TStrItemType;
begin
  p := TStrItemType(FList[Index]);
  if p <> nil then
    p.FObject := AObject
  else if AObject <> nil then
    FList[Index] := NewStrItem('',AObject);
  Changed
end;

procedure TStringSparseList.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self)
end;

procedure TStringSparseList.Delete(Index: Integer);
var
  p: TStrItemType;
begin
  p := TStrItemType(FList[Index]);
  if p <> nil then DisposeStrItem(p);
  FList.Delete(Index);
  Changed
end;

procedure TStringSparseList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TStringSparseList.Insert(Index: Integer; const S: String);
begin
  FList.Insert(Index, NewStrItem(S, nil));
  Changed
end;


procedure TStringSparseList.Clear;

  function  ClearItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    DisposeStrItem(PStrItem(TheItem));    { Item guaranteed non-nil }
    Result := 0
  end;

begin
  FList.ForAll(@ClearItem);
  FList.Clear;
  Changed
end;

{ TStringGrid }

constructor TDBGRegistersView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValues := TValuesList.Create(Self);
  FValues.OnChange := OnValuesChange;
  Initialize;
end;

destructor TDBGRegistersView.Destroy;

  function FreeItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    TObject(TheItem).Free;
    Result := 0;
  end;

begin
  if FRows <> nil then
  begin
    TSparseList(FRows).ForAll(@FreeItem);
    TSparseList(FRows).Free;
  end;
  if FCols <> nil then
  begin
    TSparseList(FCols).ForAll(@FreeItem);
    TSparseList(FCols).Free;
  end;
  if FData <> nil then
  begin
    TSparseList(FData).ForAll(@FreeItem);
    TSparseList(FData).Free;
  end;
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TDBGRegistersView.ColumnMoved(FromIndex, ToIndex: Longint);

  function MoveColData(Index: Integer; ARow: TStringSparseList): Integer; far;
  begin
    ARow.Move(FromIndex, ToIndex);
    Result := 0;
  end;

begin
  TSparseList(FData).ForAll(@MoveColData);
  Invalidate;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

procedure TDBGRegistersView.RowMoved(FromIndex, ToIndex: Longint);
begin
  TSparseList(FData).Move(FromIndex, ToIndex);
  Invalidate;
  inherited RowMoved(FromIndex, ToIndex);
end;

function TDBGRegistersView.GetEditText(ACol, ARow: Longint): string;
begin
//  Result := Cells[ACol, ARow];
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TDBGRegistersView.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  DisableEditUpdate;
  try
//    if Value <> Cells[ACol, ARow] then
//      Cells[ACol, ARow] := Value;
  finally
    EnableEditUpdate;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TDBGRegistersView.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  oldFontColor: TColor;
  data: string;
  b: boolean;
begin
  // ����� ������ �� �����
  if DefaultDrawing then
  begin
    if ARow >= FValues.Count then Exit;

    if ACol = 0 then
    begin
      data := FValues.Select(ARow).Caption;
      Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, data);
    end
    else
    if ACol - 1 < FValues.Select(ARow).ValCount then
    begin
      data := FValues.Select(ARow).GetValue(ACol - 1);

      if (FValues.Mode = 0) and (ARow < FValues.FAdditionalLines.Count) then
        Delete(data, 1, 4);

      oldFontColor := Canvas.Font.Color;

      b := FValues.IsChanged;
      if b then Canvas.Font.Color := FChangedValueColor;

      Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, data);

      if b then Canvas.Font.Color := oldFontColor;
    end;

  end;
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TDBGRegistersView.DisableEditUpdate;
begin
  Inc(FEditUpdate);
end;

procedure TDBGRegistersView.EnableEditUpdate;
begin
  Dec(FEditUpdate);
end;

procedure TDBGRegistersView.Initialize;
var
  quantum: TSPAQuantum;
begin
  if FCols = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    FCols := TSparseList.Create(quantum);
  end;
  if RowCount > 256 then quantum := SPALarge else quantum := SPASmall;
  if FRows = nil then FRows := TSparseList.Create(quantum);
  if FData = nil then FData := TSparseList.Create(quantum);
end;

procedure TDBGRegistersView.OnValuesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TDBGRegistersView.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if not Updating and FNeedsUpdating then
  begin
    InvalidateGrid;
    FNeedsUpdating := False;
  end;
end;

procedure TDBGRegistersView.Update(ACol, ARow: Integer);
begin
  if not FUpdating then InvalidateCell(ACol, ARow)
  else FNeedsUpdating := True;
  if (ACol = Col) and (ARow = Row) and (FEditUpdate = 0) then InvalidateEditor;
end;

function  TDBGRegistersView.EnsureDataRow(ARow: Integer): TCustomData;
var
  quantum: TSPAQuantum;
begin
  Result := TStringSparseList(TSparseList(FData)[ARow]);
  if Result = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    Result := TStringSparseList.Create(quantum);
    TSparseList(FData)[ARow] := Result;
  end;
end;

{ TValuesList }

function TValuesList.AddLine(const aCaption: string;
  const aAdditionalCaption: string = ''): TValuesList;
begin
  FCurrentLine := TStringList.Create;
  FCurrentPos := -1;
  FLines.AddObject(aCaption, FCurrentLine);
  if aAdditionalCaption <> '' then
    FAdditionalLines.Add(aAdditionalCaption);
  Result := Self;
end;

function TValuesList.AddValue(const aValue: string): TValuesList;
begin
  FCurrentPos := FCurrentLine.AddObject(aValue, TObject(1));
  Result := Self;
end;

function TValuesList.Caption: string;
begin
  if (FMode = 1) or (FSelectedLine >= FAdditionalLines.Count) then
    Result := FLines[FSelectedLine]
  else
    Result := FAdditionalLines[FSelectedLine];
end;

procedure TValuesList.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TValuesList.Commit;
var
  i, j: integer;
  _tmp: TStringList;
begin
  for i := 0 to pred(Count) do
  begin
    _tmp := TStringList(FLines.Objects[i]);
    for j := 0 to pred(_tmp.Count) do
      _tmp.Objects[j] := nil;
  end;
  Change;
end;

function TValuesList.Count: integer;
begin
  Result := FLines.Count;
end;

constructor TValuesList.Create(AOwner: TComponent);
begin
  FMode := 1; // 1 - 32 bit mode 0 - 16 bit mode
  FOwner := AOwner;
  FLines := TStringList.Create;
  FAdditionalLines := TStringList.Create;
end;

destructor TValuesList.Destroy;
var
  i: Integer;
begin
  for i := 0 to pred(Count) do
    FLines.Objects[i].Free;

  FLines.Free;
  FAdditionalLines.Free;
end;

function TValuesList.EOF: boolean;
begin
  Result := FCurrentPos >= FCurrentLine.Count;
end;

function TValuesList.GetValue(aIndex: integer): string;
begin
  FCurrentPos := aIndex;
  Result := FCurrentLine[FCurrentPos];
end;

function TValuesList.GetValue: string;
begin
  Result := FCurrentLine[FCurrentPos];
end;

function TValuesList.IsChanged: boolean;
begin
  Result := FCurrentLine.Objects[FCurrentPos] <> nil;
end;

function TValuesList.Next: TValuesList;
begin
  inc(FCurrentPos);
  Result := Self;
end;

function TValuesList.Select(aIndex: integer): TValuesList;
begin
  FSelectedLine := aIndex;
  FCurrentLine := TStringList(FLines.Objects[FSelectedLine]);
  Result := Self;
end;

function TValuesList.Select(const aCaption: string): TValuesList;
begin
  FSelectedLine := FLines.IndexOf(aCaption);
  FCurrentLine := TStringList(FLines.Objects[FSelectedLine]);
  Result := Self;
end;

function TValuesList.UpdateValue(const aValue: string): TValuesList;
begin
  if FCurrentLine[FCurrentPos] <> aValue then
  begin
    FCurrentLine[FCurrentPos] := aValue;
    FCurrentLine.Objects[FCurrentPos] := TObject(1);
    Change;
  end;
  Result := Self;
end;

function TValuesList.ValCount: integer;
begin
  Result := FCurrentLine.Count;
end;

{ TDBGStackView }

procedure TDBGStackView.ColumnMoved(FromIndex, ToIndex: Integer);

  function MoveColData(Index: Integer; ARow: TStringSparseList): Integer; far;
  begin
    ARow.Move(FromIndex, ToIndex);
    Result := 0;
  end;

begin
  TSparseList(FData).ForAll(@MoveColData);
  Invalidate;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

constructor TDBGStackView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStackHeadAddress := -1;
  FHeadCellHighlightColor := clYellow;
  FHeadCellHighlightFontColor := clRed;
  FBytesPerRow := 4;
  FMemoryDataSize := 0;
  FMemoryData := nil;
  Initialize;
  SetRowCount(0);
end;

destructor TDBGStackView.Destroy;

  function FreeItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    TObject(TheItem).Free;
    Result := 0;
  end;

begin
  if FRows <> nil then
  begin
    TSparseList(FRows).ForAll(@FreeItem);
    TSparseList(FRows).Free;
  end;
  if FCols <> nil then
  begin
    TSparseList(FCols).ForAll(@FreeItem);
    TSparseList(FCols).Free;
  end;
  if FData <> nil then
  begin
    TSparseList(FData).ForAll(@FreeItem);
    TSparseList(FData).Free;
  end;
  inherited Destroy;
end;

procedure TDBGStackView.DisableEditUpdate;
begin
  Inc(FEditUpdate);
end;

procedure TDBGStackView.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  oldColor, oldFontColor: TColor;
  dataStr: string;
  memOffset: Integer;
  dataByte: Byte;
  isHead: Boolean;
begin
  // ����� ������ �� �����
  if DefaultDrawing then
  begin
    if FMemoryData = nil then
      exit;
    case ACol of
      0: {������}
        begin
          isHead := (FStackHeadAddress >= ARow * FBytesPerRow) and
                    (FStackHeadAddress <  ARow * FBytesPerRow + FBytesPerRow);

          if isHead then
          begin
            oldColor := Canvas.Brush.Color;
            oldFontColor := Canvas.Font.Color;
            Canvas.Brush.Color := FHeadCellHighlightColor;
            Canvas.Font.Color := FHeadCellHighlightFontColor;
          end;

          Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top,
                          IntToHex(ARow * FBytesPerRow, 8));

          if isHead then
          begin
            Canvas.Brush.Color := oldColor;
            Canvas.Font.Color := oldFontColor;
          end;
        end;
      1: {��������}
        begin
          dataStr := '';
          for memOffset := 0 to pred(FBytesPerRow) do
          begin
            dataByte := Byte(Pointer(Cardinal(FMemoryData) + memOffset +
                                     ARow * FBytesPerRow)^);
            dataStr := IntToHex(dataByte, 2) + dataStr;
          end;
          Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, dataStr);
        end;
      2: {����������}
        begin

        end;
    end;
  end;
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TDBGStackView.EnableEditUpdate;
begin
  Dec(FEditUpdate);
end;

function TDBGStackView.EnsureDataRow(ARow: Integer): TCustomData;
var
  quantum: TSPAQuantum;
begin
  Result := TStringSparseList(TSparseList(FData)[ARow]);
  if Result = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    Result := TStringSparseList.Create(quantum);
    TSparseList(FData)[ARow] := Result;
  end;
end;

function TDBGStackView.GetEditText(ACol, ARow: Integer): string;
begin
//  Result := Cells[ACol, ARow];
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TDBGStackView.Initialize;
var
  quantum: TSPAQuantum;
begin
  if FCols = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    FCols := TSparseList.Create(quantum);
  end;
  if RowCount > 256 then quantum := SPALarge else quantum := SPASmall;
  if FRows = nil then FRows := TSparseList.Create(quantum);
  if FData = nil then FData := TSparseList.Create(quantum);
end;

procedure TDBGStackView.OnValuesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TDBGStackView.RowMoved(FromIndex, ToIndex: Integer);
begin
  TSparseList(FData).Move(FromIndex, ToIndex);
  Invalidate;
  inherited RowMoved(FromIndex, ToIndex);
end;

procedure TDBGStackView.SetBytesPerRow(const aValue: integer);
begin
  if FBytesPerRow <> aValue then
  begin
    FBytesPerRow := aValue;

    if FBytesPerRow <> 0 then
      SetRowCount(FMemoryDataSize div FBytesPerRow);

    Invalidate;
  end;
end;

procedure TDBGStackView.SetEditText(ACol, ARow: Integer; const Value: string);
begin
  DisableEditUpdate;
  try
//    if Value <> Cells[ACol, ARow] then
//      Cells[ACol, ARow] := Value;
  finally
    EnableEditUpdate;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TDBGStackView.SetHeadCellHighlightColor(const Value: TColor);
begin
  if FHeadCellHighlightColor <> Value then
  begin
    FHeadCellHighlightColor := Value;
    Invalidate;
  end;
end;

procedure TDBGStackView.SetHeadCellHighlightFontColor(const Value: TColor);
begin
  if FHeadCellHighlightFontColor <> Value then
  begin
    FHeadCellHighlightFontColor := Value;
    Invalidate;
  end;
end;

procedure TDBGStackView.SetMemoryData(const aValue: pointer);
begin
  if aValue <> FMemoryData then
  begin
    FMemoryData := aValue;
    FMemoryDataSize := 0;
    FRowCount := 0;
    Invalidate;
  end;
end;

procedure TDBGStackView.SetMemoryDataSize(const Value: Int64);
begin
  if FMemoryDataSize <> Value then
  begin
    FMemoryDataSize := Value;

    if FBytesPerRow <> 0 then
      SetRowCount(FMemoryDataSize div FBytesPerRow);

    Invalidate;
  end;
end;

procedure TDBGStackView.SetStackHeadAddress(const aValue: Int64);
begin
  if aValue <> FStackHeadAddress then
  begin
    FStackHeadAddress := aValue;
    SetRow(FStackHeadAddress div FBytesPerRow);
    Invalidate;
  end;
end;

procedure TDBGStackView.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if not Updating and FNeedsUpdating then
  begin
    InvalidateGrid;
    FNeedsUpdating := False;
  end;
end;

procedure TDBGStackView.Update(ACol, ARow: Integer);
begin
  if not FUpdating then InvalidateCell(ACol, ARow)
  else FNeedsUpdating := True;
  if (ACol = Col) and (ARow = Row) and (FEditUpdate = 0) then InvalidateEditor;
end;

{ TDBGCPUView }

procedure TDBGCPUView.ColumnMoved(FromIndex, ToIndex: Integer);

  function MoveColData(Index: Integer; ARow: TStringSparseList): Integer; far;
  begin
    ARow.Move(FromIndex, ToIndex);
    Result := 0;
  end;

begin
  TSparseList(FData).ForAll(@MoveColData);
  Invalidate;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

constructor TDBGCPUView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCodeSegmentValue := 0;
  FCurrentLine := -1;
  FMemoryDataSize := 0;
  FMemoryData := nil;
  Initialize;
  SetRowCount(0);
  FDisasm := TMiniDisasm.Create;
  FDisasm.OnParseCode := DisasmOnParse;
end;

destructor TDBGCPUView.Destroy;

  function FreeItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    TObject(TheItem).Free;
    Result := 0;
  end;

begin
  if FRows <> nil then
  begin
    TSparseList(FRows).ForAll(@FreeItem);
    TSparseList(FRows).Free;
  end;
  if FCols <> nil then
  begin
    TSparseList(FCols).ForAll(@FreeItem);
    TSparseList(FCols).Free;
  end;
  if FData <> nil then
  begin
    TSparseList(FData).ForAll(@FreeItem);
    TSparseList(FData).Free;
  end;
  FDisasm.Free;
  inherited Destroy;
end;

procedure TDBGCPUView.DisableEditUpdate;
begin
  Inc(FEditUpdate);
end;

procedure TDBGCPUView.DisasmOnParse(aDataPtr: pointer;
  var aItemInfo: TDisasmItem);
begin
  if Assigned(FOnParseCode) then
    FOnParseCode(aDataPtr, aItemInfo);
end;

procedure TDBGCPUView.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  dataStr: string;
  dataByte: Byte;
  beforeCS,
  afterCS: boolean;
  _item: TDisasmItem;
  isEIP: boolean;
  oldColor, oldFontColor: TColor;
begin
  // ����� ������ �� �����
  if DefaultDrawing then
  begin
    if FMemoryData = nil then
      exit;

    beforeCS := ARow < FDisasm.SegmentOffset;
    afterCS := ARow > (FDisasm.SegmentOffset + FDisasm.Count) - 1;
    case ACol of
      0: {������}
        begin
          // ����� ��������
          if beforeCS then
            Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, IntToHex(ARow, 8))
          else
          // �� ���������
          if afterCS then
            Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, IntToHex(ARow +
                (FDisasm.SegmentOffsetSize - FDisasm.Count), 8))
          else
          // � ��������
          begin
            _item := FDisasm.GetItem(ARow - FDisasm.SegmentOffset);
            isEIP := FCurrentLine = _item.Address;

            if isEIP then
            begin
              oldColor := Canvas.Brush.Color;
              oldFontColor := Canvas.Font.Color;
              Canvas.Brush.Color := FEIPCellHighlightColor;
              Canvas.Font.Color := FEIPCellHighlightFontColor;
            end;

            dataStr := IntToHex(_item.Address, 8);
            Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, dataStr);

            if isEIP then
            begin
              Canvas.Brush.Color := oldColor;
              Canvas.Font.Color := oldFontColor;
            end;
          end;
        end;
      1: {������}
        begin
          if beforeCS then
          begin
            dataByte := Byte(Pointer(Cardinal(FMemoryData) + ARow)^);
            dataStr := IntToHex(dataByte, 2);
            Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, dataStr);
          end else
          if afterCS then
          begin
            dataByte := Byte(Pointer(Cardinal(FMemoryData) + (FDisasm.SegmentOffsetSize - FDisasm.Count) + ARow)^);
            dataStr := IntToHex(dataByte, 2);
            Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, dataStr);
          end else
          begin
            _item := FDisasm.GetItem(ARow - FDisasm.SegmentOffset);
            dataStr := _item.Opcode;
            Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, dataStr);
          end;
        end;
      2: {������}
        begin
          // ������ ����� ���������� ������ �������� � �������� ����
          if (not beforeCS) and (not afterCS) then
          begin
            _item := FDisasm.GetItem(ARow - FDisasm.SegmentOffset);
            dataStr := _item.Disasm;
            Canvas.TextRect(ARect, ARect.Left + 3, ARect.Top, dataStr);
          end;
        end;
    end;
  end;
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TDBGCPUView.EnableEditUpdate;
begin
  Dec(FEditUpdate);
end;

function TDBGCPUView.EnsureDataRow(ARow: Integer): TCustomData;
var
  quantum: TSPAQuantum;
begin
  Result := TStringSparseList(TSparseList(FData)[ARow]);
  if Result = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    Result := TStringSparseList.Create(quantum);
    TSparseList(FData)[ARow] := Result;
  end;
end;

function TDBGCPUView.GetEditText(ACol, ARow: Integer): string;
begin
//  Result := Cells[ACol, ARow];
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TDBGCPUView.Initialize;
var
  quantum: TSPAQuantum;
begin
  if FCols = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    FCols := TSparseList.Create(quantum);
  end;
  if RowCount > 256 then quantum := SPALarge else quantum := SPASmall;
  if FRows = nil then FRows := TSparseList.Create(quantum);
  if FData = nil then FData := TSparseList.Create(quantum);
end;

procedure TDBGCPUView.OnValuesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TDBGCPUView.RowMoved(FromIndex, ToIndex: Integer);
begin
  TSparseList(FData).Move(FromIndex, ToIndex);
  Invalidate;
  inherited RowMoved(FromIndex, ToIndex);
end;

procedure TDBGCPUView.SetCodeSegmentSize(const Value: Int64);
begin
  if FCodeSegmentSize <> Value then
  begin
    FCodeSegmentSize := Value;

    Invalidate;
  end;
end;

procedure TDBGCPUView.SetCodeSegmentValue(const Value: Int64);
begin
  if FCodeSegmentValue <> Value then
  begin
    FCodeSegmentValue := Value;

    Invalidate;
  end;
end;

procedure TDBGCPUView.SetCurrentLine(const Value: integer);
begin
  // ��� �� ���� eip
  if FCurrentLine <> Value then
  begin
    FCurrentLine := Value;
    SetRow(FDisasm.SegmentOffset + FDisasm.IndexOfAddress(Value));

    Invalidate;
  end;
end;

procedure TDBGCPUView.SetEditText(ACol, ARow: Integer; const Value: string);
begin
  DisableEditUpdate;
  try
//    if Value <> Cells[ACol, ARow] then
//      Cells[ACol, ARow] := Value;
  finally
    EnableEditUpdate;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TDBGCPUView.SetEIPCellHighlightColor(const Value: TColor);
begin
  if FEIPCellHighlightColor <> Value then
  begin
    FEIPCellHighlightColor := Value;

    Invalidate;
  end;
end;

procedure TDBGCPUView.SetEIPCellHighlightFontColor(const Value: TColor);
begin
  if FEIPCellHighlightFontColor <> Value then
  begin
    FEIPCellHighlightFontColor := Value;

    Invalidate;
  end;
end;

procedure TDBGCPUView.SetMemoryData(const aValue: pointer);
begin
  if aValue <> FMemoryData then
  begin
    FMemoryData := aValue;
    FMemoryDataSize := 0;
    FRowCount := 0;
    Invalidate;
  end;
end;

procedure TDBGCPUView.SetMemoryDataSize(const Value: Int64);
begin
  if FMemoryDataSize <> Value then
  begin
    FMemoryDataSize := Value;

    SetRowCount(FMemoryDataSize);

    Invalidate;
  end;
end;

procedure TDBGCPUView.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if not Updating and FNeedsUpdating then
  begin
    InvalidateGrid;
    FNeedsUpdating := False;
  end;
end;

procedure TDBGCPUView.Update(ACol, ARow: Integer);
begin
  if not FUpdating then InvalidateCell(ACol, ARow)
  else FNeedsUpdating := True;
  if (ACol = Col) and (ARow = Row) and (FEditUpdate = 0) then InvalidateEditor;
end;

procedure TDBGCPUView.UpdateDisasm;
begin
  FDisasm.MemoryData := FMemoryData;
  FDisasm.MemoryDataSize := FMemoryDataSize;
  FDisasm.SegmentOffset := FCodeSegmentValue;
  FDisasm.SegmentOffsetSize := FCodeSegmentSize;
  FDisasm.Parse;
end;

{ TMiniDisasm }

procedure TMiniDisasm.AddItem(const aItem: TDisasmItem);
var
  _item: PDisasmItem;
begin
  new(_item);
  _item^.Address := aItem.Address;
  _item^.Size := aItem.Size;
  _item^.Opcode := aItem.Opcode;
  _item^.Disasm := aItem.Disasm;

  FList.Add(_item);
end;

procedure TMiniDisasm.AfterConstruction;
begin
  inherited;
  FList := TList.Create;
end;

procedure TMiniDisasm.BeforeDestruction;
begin
  inherited;
  Clear;
  FList.Free;
end;

procedure TMiniDisasm.Clear;
begin
  while FList.Count <> 0 do
  begin
    FreeMemory(FList[FList.Count - 1]);
    FList.Delete(FList.Count - 1);
  end;
end;

function TMiniDisasm.Count: integer;
begin
  Result := FList.Count;
end;

function TMiniDisasm.GetItem(const index: integer): TDisasmItem;
begin
  Result := PDisasmItem(FList[index])^;
end;

function TMiniDisasm.IndexOfAddress(const addr: int64): int64;
var
  i: integer;
begin
  if (FSegmentOffset + addr) > ((FSegmentOffset + FSegmentOffsetSize) - addr) then
    for i := pred(FList.Count) downto 0 do
    begin
      if TDisasmItem(FList[i]^).Address = addr then
        exit(i);
    end
  else
    for i := 0 to pred(FList.Count) do
    begin
      if TDisasmItem(FList[i]^).Address = addr then
        exit(i);
    end;
end;

procedure TMiniDisasm.Parse;
var
  i: integer;
  _item: TDisasmItem;
begin
  // ������ ������ ������� FSegmentOffset..FSegmentOffset+FSegmentOffsetSize
  Clear;
  // FSegmentOffset - �������� ������������ ������ ����� ����� ������ (CS)
  // ����� (����������) ������� ����������������� ������� == FSegmentOffset + i
  i := 0;
  while i < FSegmentOffsetSize do
  begin
    FillChar(_item, SizeOf(TDisasmItem), 0);

    if Assigned(FOnParseCode) then
    begin
      _item.Address := FSegmentOffset + i;
      _item.Segment := FSegmentOffset;
      FOnParseCode(Pointer(_item.Address), _item);
      AddItem(_item);
    end;

    inc(i, max(_item.Size, 1));
  end;
end;

procedure TMiniDisasm.SetMemoryData(const Value: pointer);
begin
  FMemoryData := Value;
end;

procedure TMiniDisasm.SetMemoryDataSize(const Value: Int64);
begin
  FMemoryDataSize := Value;
end;

procedure TMiniDisasm.SetSegmentOffset(const Value: int64);
begin
  FSegmentOffset := Value;
end;

procedure TMiniDisasm.SetSegmentOffsetSize(const Value: int64);
begin
  FSegmentOffsetSize := Value;
end;

end.
