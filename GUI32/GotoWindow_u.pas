unit GotoWindow_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TOnGetVarValue = function(Sender: TObject; const Variable: string;
    var Value: Cardinal): Boolean of object;

  TGoToForm = class(TForm)
    cbbValue: TComboBox;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function GoToExpression(aOwner: TComponent; aList: TStringList;
      aDefVal: string; const OnGetVarVal: TOnGetVarValue; var aVal: Cardinal): boolean;
  end;

var
  GoToForm: TGoToForm;

implementation

{$R *.dfm}

type
  TNode = class
  public
    Left: TNode;
    Right: TNode;

    Val: Cardinal;
    Oper: Byte;

    destructor Destroy; override;
  end;

  TExprParser = class
  private
    FTree,
    FNode: TNode;
    FOnGetVarVal: TOnGetVarValue;
    function Parse(const aExpr: string; const aNode: TNode): boolean;
    function Calc(const aNode: TNode): Cardinal;
  public
    property OnGetVarVal: TOnGetVarValue read FOnGetVarVal write FOnGetVarVal;
    function ExprToVal(const aExpr: string): Cardinal;
  end;

{ TGoToForm }

procedure TGoToForm.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

class function TGoToForm.GoToExpression(aOwner: TComponent; aList: TStringList;
  aDefVal: string; const OnGetVarVal: TOnGetVarValue; var aVal: Cardinal): Boolean;
var
  _frm: TGoToForm;
  _calc: TExprParser;
begin
  _frm := TGoToForm.Create(aOwner);
  _calc := TExprParser.Create;
  _calc.OnGetVarVal := OnGetVarVal;
  Result := False;
  try
    if aList <> nil then
      _frm.cbbValue.Items.Assign(aList);
    _frm.cbbValue.Text := aDefVal;
    Result := _frm.ShowModal = mrOk;
    if Result then
      aVal := _calc.ExprToVal(_frm.cbbValue.Text);
  finally
    _frm.Free;
    _calc.Free;
  end;
end;

{ TNode }

destructor TNode.Destroy;
begin
  if Left <> nil then
    Left.Free;
  if Right <> nil then
    Right.Free;
  inherited;
end;

{ TExprParser }

function TExprParser.Calc(const aNode: TNode): Cardinal;
begin
  if aNode.Oper in [0, 1] then
    Exit(aNode.Val);
  if aNode.Oper = 2 then
    Exit(calc(aNode.Left) + calc(aNode.Right));
  if aNode.Oper = 3 then
    Exit(calc(aNode.Left) - calc(aNode.Right));
  if aNode.Oper = 4 then
    Exit(calc(aNode.Left) * calc(aNode.Right));
  if aNode.Oper = 5 then
    Exit(calc(aNode.Left) div calc(aNode.Right));
end;

function TExprParser.ExprToVal(const aExpr: string): Cardinal;
var
  _node: TNode;
begin
  _node := TNode.Create;
  try
    if Parse(aExpr, _node) then
      Result := Calc(_node);
  finally
    _node.Free;
  end;
end;

function TExprParser.Parse(const aExpr: string; const aNode: TNode): boolean;

  function SearchOp(const str, op: string): Integer;
  var
    c, i, d: Integer;
  begin
    c :=0;
    d := 0;
    for i := Length(str) downto 1 do
    begin
      if str[i] = #$29 then Inc(c)
      else
      if str[i] = #$28 then Dec(c);

      if str[i] = #$5D then Inc(d)
      else
      if str[i] = #$5B then Dec(d)
      else
      if Copy(str, i, Length(op)) = op then
        if (c = 0) and (d = 0) then
          Exit(i);
    end;
    Result := -1;
  end;

  function recurse(const str: string; oppos, oplen, optype: Integer): Boolean;
  var
    l, r: string;
  begin
    l := Copy(str, 1, oppos - oplen);
    r := copy(str, oppos + oplen, Length(str)-oppos);
    aNode.Left := TNode.Create;
    aNode.Right := TNode.Create;
    aNode.Oper := optype;
    Exit(Parse(l, aNode.Left) and Parse(r, aNode.Right));
  end;

var
  s: string;
  i: Integer;
  _val: Cardinal;
  _code: Integer;
begin
  s := LowerCase(aExpr);
  if s = '' then
    exit(False);
  if s[1] = '-' then
    s := '0' + s;
  if Assigned(FOnGetVarVal) and FOnGetVarVal(Self, s, _val) then
  begin
    aNode.Oper := 0;
    aNode.val := _val;
    Exit(True);
  end;
  val(s, _val, _code);
  if _code = 0 then
  begin
    aNode.Oper := 1;
    aNode.Val := _val;
    Exit(True);
  end;
  i := SearchOp(s, '+');
  if i > 0 then Exit(recurse(s, i, 1, 2));
  i := SearchOp(s, '-');
  if i > 0 then Exit(recurse(s, i, 1, 3));
  i := SearchOp(s, '*');
  if i > 0 then Exit(recurse(s, i, 1, 4));
  i := SearchOp(s, '/');
  if i > 0 then Exit(recurse(s, i, 1, 5));

  if (s[1] = '(') and (s[Length(s)] = ')') then
  begin
    s := Copy(s, 2, Length(s) - 2);
    Exit(Parse(s, aNode));
  end;
  if s[1]=' ' then
  begin
    s := Copy(s, 2, Length(s) - 1);
    Exit(Parse(s, aNode));
  end;
  if s[length(s)]=' ' then
  begin
    s := Copy(s, 1, Length(s) - 1);
    Exit(Parse(s, aNode));
  end;
end;

end.
