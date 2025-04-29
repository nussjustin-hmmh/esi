// TODO: implement esiproc.Env
// TODO: implement esiproc.WithEnv
// TODO: remove esiproc.TestFunc
// TODO: remove esiproc.WithTestFunc
// TODO: update README
package esiexpr

import (
	"fmt"
	"io"
	"strconv"
	"sync"
)

// SyntaxError is returned by [Parse] and [ParseVariable] when encountering unexpected or invalid data.
type SyntaxError struct {
	// Offset is the position in the input where the error occurred.
	Offset int

	// Message may contain a custom message that describes the error
	Message string

	// Underlying optionally contains the underlying error that lead to this error.
	Underlying error
}

// Error returns a human-readable error message.
func (s *SyntaxError) Error() string {
	if s.Message == "" {
		return fmt.Sprintf("invalid syntax at offset %d", s.Offset)
	}

	return fmt.Sprintf("invalid syntax at offset %d: %s", s.Offset, s.Message)
}

// Unwrap returns s.Underlying.
func (s *SyntaxError) Unwrap() error {
	return s.Underlying
}

// Node is the interface implemented by all possible types of parsed nodes.
type Node interface {
	node()
}

// AndNode represents two sub-expressions combined with the and operator (&).
type AndNode struct {
	// Left contains the expression to the left of the operator.
	Left Node

	// Right contains the expression to the right of the operator.
	Right Node
}

func (*AndNode) node() {}

// ComparisonNode represents a comparison between two values using one of the supported comparison operators.
type ComparisonNode struct {
	// Operator contains the parsed operator.
	Operator ComparisonOperator

	// Left contains the expression to the left of the operator.
	Left Node

	// Right contains the expression to the right of the operator.
	Right Node
}

func (*ComparisonNode) node() {}

// ComparisonOperator is an enum of supported comparison operators.
type ComparisonOperator string

const (
	// ComparisonOperatorEquals is the type for comparisons using the "==" operator.
	ComparisonOperatorEquals ComparisonOperator = "=="

	// ComparisonOperatorGreaterThan is the type for comparisons using the ">" operator.
	ComparisonOperatorGreaterThan ComparisonOperator = ">"

	// ComparisonOperatorGreaterThanEquals is the type for comparisons using the ">=" operator.
	ComparisonOperatorGreaterThanEquals ComparisonOperator = ">="

	// ComparisonOperatorLessThan is the type for comparisons using the "<=>=" operator.
	ComparisonOperatorLessThan ComparisonOperator = "<"

	// ComparisonOperatorLessThanEquals is the type for comparisons using the "<=>=" operator.
	ComparisonOperatorLessThanEquals ComparisonOperator = "<="

	// ComparisonOperatorNotEquals is the type for comparisons using the "!=" operator.
	ComparisonOperatorNotEquals ComparisonOperator = "!="
)

// NegateNode represents a sub-expression negated using the unary negation operator (!).
type NegateNode struct {
	// Expr is the negated sub-expression.
	Expr Node
}

func (*NegateNode) node() {}

// OrNode represents two sub-expressions combined with the or operator (|).
type OrNode struct {
	// Left contains the expression to the left of the operator.
	Left Node

	// Right contains the expression to the right of the operator.
	Right Node
}

func (*OrNode) node() {}

// Scalar contains a single parsed float, int or string value.
type Scalar struct {
	// Type determines the type of the value.
	Type ScalarType

	// String contains the parsed value iff Type is ScalarTypeFloat.
	Float float64

	// String contains the parsed iff Type is ScalarTypeInt.
	Int int64

	// String contains the parsed iff Type is ScalarTypeString.
	String string
}

// ScalarNode represents a scalar value, which is either a string or number.
type ScalarNode struct {
	// Scalar contains the parsed value.
	Scalar Scalar
}

func (*ScalarNode) node() {}

// ScalarType is an enum of possible scalar types.
type ScalarType uint8

const (
	// ScalarTypeInvalid is the zero value for ScalarType and is not a valid type.
	ScalarTypeInvalid ScalarType = iota

	// ScalarTypeFloat is used for scalar numbers with a decimal component.
	ScalarTypeFloat

	// ScalarTypeInt is used for scalar numbers without a decimal component.
	ScalarTypeInt

	// ScalarTypeNull is used for scalar null values.
	ScalarTypeNull

	// ScalarTypeString is used for scalar string values.
	ScalarTypeString
)

// VariableNode represents a variable reference including its default value, if any.
type VariableNode struct {
	// Name contains the parsed variable name.
	Name string

	// Key is the name of the key inside the referenced dictionary or list.
	Key *string

	// Default contains the default value, if any.
	Default Scalar
}

func (*VariableNode) node() {}

var parserPool = sync.Pool{
	New: func() any {
		return &parser{}
	},
}

func getParser(data string) *parser {
	p, _ := parserPool.Get().(*parser)
	p.reset(data)
	return p
}

func putParser(p *parser) {
	p.reset("")
	parserPool.Put(p)
}

// Parse parses the given ESI expression into a tree of nodes.
func Parse(data string) (Node, error) {
	p := getParser(data)
	defer putParser(p)

	// TODO: Test
	node, err := p.parseExpr()
	if err != nil {
		return nil, err
	}

	// TODO: Test
	p.discardSpaces()

	// TODO: Test
	if p.offset < len(p.data) {
		return nil, &SyntaxError{Offset: p.offset, Message: "unexpected data after expression"}
	}

	// TODO: Test
	return node, nil
}

// ParseVariable parses an ESI variable from the given string.
func ParseVariable(data string) (*VariableNode, error) {
	p := getParser(data)
	defer putParser(p)

	// TODO: Test
	node, err := p.parseVar()
	if err != nil {
		return nil, err
	}

	// TODO: Test
	if p.offset < len(p.data) {
		return nil, &SyntaxError{Offset: p.offset, Message: "unexpected data after variable"}
	}

	// TODO: Test
	return node.(*VariableNode), nil
}

type parser struct {
	data   string
	offset int
}

func (p *parser) consume(c byte) error {
	c1, err := p.next()
	if err != nil {
		// TODO: Test
		return err
	}
	if c1 != c {
		// TODO: Test
		return &SyntaxError{
			Offset:  p.offset,
			Message: "unexpected character '" + string(rune(c1)) + "', '" + string(rune(c)) + "' expected",
		}
	}
	return nil
}

func (p *parser) consumeIfNext(c byte) bool {
	if p.offset >= len(p.data) || p.data[p.offset] != c {
		return false
	}
	p.offset++
	return true
}

func (p *parser) discardSpaces() {
	for p.offset < len(p.data) {
		switch p.data[p.offset] {
		case ' ', '\r', '\n', '\t':
			p.offset++
		default:
			return
		}
	}
}

func (p *parser) next() (byte, error) {
	if p.offset >= len(p.data) {
		// TODO: Test
		return 0, &SyntaxError{Offset: p.offset, Underlying: io.ErrUnexpectedEOF}
	}
	p.offset++
	return p.data[p.offset-1], nil
}

func (p *parser) peek() (byte, bool) {
	if p.offset >= len(p.data) {
		return 0, false
	}
	return p.data[p.offset], true
}

func (p *parser) unread() {
	p.offset--
}

func (p *parser) readIdentifier() (string, error) {
	start := p.offset

loop:
	for {
		// TODO: Test
		c, ok := p.peek()
		if !ok {
			break
		}

		_ = p.consume(c)

		// TODO: Test
		switch {
		case c >= '0' && c <= '9', c >= 'a' && c <= 'z', c >= 'A' && c <= 'Z', c == '_':
		default:
			break loop
		}
	}

	return p.data[start:p.offset], nil
}

func (p *parser) readIdentifierOrQuotedString() (string, error) {
	if c, _ := p.peek(); c == '\'' {
		return p.readQuotedString()
	}
	return p.readIdentifier()
}

func (p *parser) readQuotedString() (string, error) {
	if err := p.consume('\''); err != nil {
		return "", err
	}

	start := p.offset

	for {
		// TODO: Test
		c, err := p.next()
		if err != nil {
			return "", err
		}

		// TODO: Test
		if c == '\'' {
			break
		}
	}

	return p.data[start : p.offset-1], nil
}

func (p *parser) parseComparison() (Node, error) {
	// TODO: Test
	left, err := p.parseScalarOrVar()
	if err != nil {
		return nil, err
	}

	// TODO: Test
	p.discardSpaces()

	// TODO: Test
	op, err := p.parseOperator()
	if err != nil {
		return nil, err
	}

	// TODO: Test
	p.discardSpaces()

	// TODO: Test
	right, err := p.parseScalarOrVar()
	if err != nil {
		return nil, err
	}

	return &ComparisonNode{
		Operator: op,
		Left:     left,
		Right:    right,
	}, nil
}

func (p *parser) parseExpr() (Node, error) {
	// TODO: Test
	p.discardSpaces()

	var node Node
	var err error

	switch c, _ := p.peek(); c {
	case '!':
		node, err = p.parseNegation()
	case '(':
		node, err = p.parseSubExpr()
	default:
		node, err = p.parseComparison()
	}

	if err != nil {
		return nil, err
	}

	// TODO: Test
	p.discardSpaces()

	switch {
	case p.consumeIfNext('|'):
		// TODO: Test (invalid after |)
		node1, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		// TODO: Test
		return &OrNode{Left: node, Right: node1}, nil
	case p.consumeIfNext('&'):
		// TODO: Test (invalid after &)
		node1, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		// TODO: Test
		return &AndNode{Left: node, Right: node1}, nil
	default:
		return node, nil
	}
}

// TODO: Test
// TODO: Test invalid data
func (p *parser) parseNegation() (Node, error) {
	if err := p.consume('!'); err != nil {
		return nil, err
	}

	// TODO: Test
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}

	// TODO: Test
	return &NegateNode{Expr: expr}, nil
}

func (p *parser) parseNull() (Node, error) {
	// TODO: Test
	if err := p.consume('n'); err != nil {
		return nil, err
	}

	// TODO: Test
	if err := p.consume('u'); err != nil {
		return nil, err
	}

	// TODO: Test
	if err := p.consume('l'); err != nil {
		return nil, err
	}

	// TODO: Test
	if err := p.consume('l'); err != nil {
		return nil, err
	}

	return &ScalarNode{
		Scalar: Scalar{
			Type: ScalarTypeNull,
		},
	}, nil
}

func (p *parser) parseOperator() (ComparisonOperator, error) {
	switch c, _ := p.peek(); c {
	case '=':
		if err := p.consume('='); err != nil {
			return "", err
		}
		return ComparisonOperatorEquals, nil
	case '!':
		if err := p.consume('='); err != nil {
			return "", err
		}
		return ComparisonOperatorNotEquals, nil
	case '<':
		if p.consumeIfNext('=') {
			return ComparisonOperatorLessThanEquals, nil
		}
		return ComparisonOperatorLessThan, nil
	case '>':
		if p.consumeIfNext('=') {
			return ComparisonOperatorGreaterThanEquals, nil
		}
		return ComparisonOperatorGreaterThan, nil
	default:
		return "", &SyntaxError{
			Offset:  p.offset,
			Message: "unexpected character '" + string(c) + "', one of '=', '!', '>', '<' expected",
		}
	}
}

func (p *parser) parseNumber() (Node, error) {
	start := p.offset

	s := Scalar{Type: ScalarTypeInt}

loop:
	for {
		// TODO: Test (number at the end of input)
		c, ok := p.peek()
		if !ok {
			break
		}

		_ = p.consume(c)

		switch {
		case c >= '0' && c <= '9':
		case c == '-':
			// TODO: Test -123
			// TODO: Test -1-23
			// TODO: Test 1-23
		case c == '.':
			if s.Type == ScalarTypeFloat {
				// TODO: Test
				return nil, &SyntaxError{Offset: p.offset - 1, Message: "unexpected decimal separator"}
			}

			// TODO: Test
			s.Type = ScalarTypeFloat
		default:
			break loop
		}
	}

	if s.Type == ScalarTypeFloat {
		var err error
		if s.Float, err = strconv.ParseFloat(p.data[start:p.offset], 64); err != nil {
			// TODO: Test
			return nil, &SyntaxError{Offset: start, Message: "invalid number", Underlying: err}
		}
	} else {
		var err error
		if s.Int, err = strconv.ParseInt(p.data[start:p.offset], 10, 64); err != nil {
			// TODO: Test
			return nil, &SyntaxError{Offset: start, Message: "invalid number", Underlying: err}
		}
	}

	// TODO: Test
	return &ScalarNode{Scalar: s}, nil
}

func (p *parser) parseQuotedString() (Node, error) {
	s, err := p.readQuotedString()
	if err != nil {
		return nil, err
	}

	return &ScalarNode{
		Scalar: Scalar{
			Type:   ScalarTypeString,
			String: s,
		},
	}, nil
}

func (p *parser) parseScalar() (Node, error) {
	c, err := p.next()
	if err != nil {
		return nil, err
	}
	p.unread()

	switch {
	case c == '\'':
		// TODO: Test
		return p.parseQuotedString()
	case c >= '0' && c <= '9':
		// TODO: Test
		return p.parseNumber()
	case c == 'n':
		// TODO: Test
		return p.parseNull()
	default:
		// TODO: Test
		return nil, &SyntaxError{
			Offset:  p.offset,
			Message: "unexpected character '" + string(c) + "', literal string, number of null expected",
		}
	}
}

func (p *parser) parseScalarOrVar() (Node, error) {
	if c, ok := p.peek(); ok && c == '$' {
		// TODO: Test
		return p.parseVar()
	}

	// TODO: Test
	return p.parseScalar()
}

func (p *parser) parseSubExpr() (Node, error) {
	if err := p.consume('('); err != nil {
		return nil, err
	}

	// TODO: Test
	p.discardSpaces()

	// TODO: Test
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}

	// TODO: Test
	p.discardSpaces()

	// TODO: Test
	if err := p.consume(')'); err != nil {
		return nil, err
	}

	return expr, nil
}

func (p *parser) parseVar() (Node, error) {
	// TODO: Test (ParseVariable)
	if err := p.consume('$'); err != nil {
		return nil, err
	}

	// TODO: Test
	if err := p.consume('('); err != nil {
		return nil, err
	}

	// TODO: Test
	name, err := p.readIdentifier()
	if err != nil {
		return nil, err
	}

	var key *string

	// TODO: Test
	// TODO: Test space (not allowed)
	if p.consumeIfNext('{') {
		// TODO: Test
		// TODO: Test quoted
		// TODO: Test quoted eof
		key1, err := p.readIdentifierOrQuotedString()
		if err != nil {
			return nil, err
		}

		// TODO: Test space (not allowed)

		// TODO: Test
		if err := p.consume('}'); err != nil {
			return nil, err
		}

		key = &key1
	}

	var value Scalar

	// TODO: Test
	// TODO: Test space (not allowed)
	if p.consumeIfNext('|') {
		// TODO: Test space (not allowed)

		// TODO: Test
		value1, err := p.parseScalar()
		if err != nil {
			return nil, err
		}

		// TODO: Test
		value = value1.(*ScalarNode).Scalar
	}

	// TODO: Test space (not allowed)

	// TODO: Test
	if err := p.consume(')'); err != nil {
		return nil, err
	}

	return &VariableNode{
		Name:    name,
		Key:     key,
		Default: value,
	}, nil
}

func (p *parser) reset(data string) {
	*p = parser{data: data}
}
