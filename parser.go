package esi

import (
	"errors"
	"fmt"
	"io"
	"slices"
	"sync"

	"github.com/nussjustin/esi/esixml"
)

// DuplicateElementError is returned when multiple elements with the same name are found where only one is allowed.
type DuplicateElementError struct {
	Position Position

	// Name is the element name.
	Name esixml.Name
}

// Error returns a human-readable error message.
func (d *DuplicateElementError) Error() string {
	return fmt.Sprintf(`duplicate element %s at position %s`, d.Name, d.Position)
}

// Is checks if the given error matches the receiver.
func (d *DuplicateElementError) Is(err error) bool {
	var o *DuplicateElementError
	return errors.As(err, &o) && *o == *d
}

// EmptyElementError is returned when an element that requires content is specified as self-closed.
type EmptyElementError struct {
	Position Position

	// Element is the element name.
	Name esixml.Name
}

// Error returns a human-readable error message.
func (e *EmptyElementError) Error() string {
	return fmt.Sprintf(
		`element %s is empty at position %s`,
		e.Name,
		e.Position,
	)
}

// Is checks if the given error matches the receiver.
func (e *EmptyElementError) Is(err error) bool {
	var o *EmptyElementError
	return errors.As(err, &o) && *o == *e
}

// InvalidAttributeValueError is returned when an attribute value is invalid for the specific attribute.
type InvalidAttributeValueError struct {
	Position Position

	// Element is the element name.
	Element esixml.Name

	// Name is the attribute name.
	Name esixml.Name

	// Value is the provided invalid value.
	Value string

	// Allowed contains a list of allowed values for the attribute, if known.
	Allowed []string
}

// Error returns a human-readable error message.
func (i *InvalidAttributeValueError) Error() string {
	return fmt.Sprintf(
		`invalid value %q for attribute %s in element %s at position %s`,
		i.Value,
		i.Name,
		i.Element,
		i.Position,
	)
}

// Is checks if the given error matches the receiver.
func (i *InvalidAttributeValueError) Is(err error) bool {
	var o *InvalidAttributeValueError
	return errors.As(err, &o) && o.Element == i.Element && o.Name == i.Name && o.Value == i.Value && slices.Equal(o.Allowed, i.Allowed) //nolint:lll
}

// InvalidElementError is returned when an invalid <esi:*> element is found.
type InvalidElementError struct {
	Position Position

	// Name is the element name.
	Name esixml.Name
}

// Error returns a human-readable error message.
func (i *InvalidElementError) Error() string {
	return fmt.Sprintf(`invalid element %s at position %s`, i.Name, i.Position)
}

// Is checks if the given error matches the receiver.
func (i *InvalidElementError) Is(err error) bool {
	var o *InvalidElementError
	return errors.As(err, &o) && *o == *i
}

// MissingAttributeError is returned when a required attribute is missing on an element.
type MissingAttributeError struct {
	Position Position

	// Element is the element name.
	Element esixml.Name

	// Attribute is the name of the attribute.
	Attribute esixml.Name
}

// Error returns a human-readable error message.
func (m *MissingAttributeError) Error() string {
	return fmt.Sprintf(`missing attribute %s in element %s at position %s`, m.Attribute, m.Element, m.Position)
}

// Is checks if the given error matches the receiver.
func (m *MissingAttributeError) Is(err error) bool {
	var o *MissingAttributeError
	return errors.As(err, &o) && *o == *m
}

// MissingElementError is returned when a required child element is not found inside another element.
type MissingElementError struct {
	Position Position

	// Name is the element name.
	Name esixml.Name
}

// Error returns a human-readable error message.
func (m *MissingElementError) Error() string {
	return fmt.Sprintf(`missing element %s at position %s`, m.Name, m.Position)
}

// Is checks if the given error matches the receiver.
func (m *MissingElementError) Is(err error) bool {
	var o *MissingElementError
	return errors.As(err, &o) && *o == *m
}

// UnclosedElementError is returned when an empty element is not closed directly.
type UnclosedElementError struct {
	Position Position

	// Name is the element name.
	Name esixml.Name
}

// Error returns a human-readable error message.
func (u *UnclosedElementError) Error() string {
	return fmt.Sprintf(`unclosed element %s at position %s`, u.Name, u.Position)
}

// Is checks if the given error matches the receiver.
func (u *UnclosedElementError) Is(err error) bool {
	var o *UnclosedElementError
	return errors.As(err, &o) && *o == *u
}

// UnexpectedElementError is returned when a specific element was expected, but a different one was encountered.
type UnexpectedElementError struct {
	Position Position

	// Name is the encountered element name.
	Name esixml.Name
}

// Error returns a human-readable error message.
func (u *UnexpectedElementError) Error() string {
	return fmt.Sprintf(`unexpected element %s at position %s`, u.Name, u.Position)
}

// Is checks if the given error matches the receiver.
func (u *UnexpectedElementError) Is(err error) bool {
	var o *UnexpectedElementError
	return errors.As(err, &o) && *o == *u
}

// UnexpectedEndElementError is returned when an end-element is found that does not match the currently open element.
type UnexpectedEndElementError struct {
	Position Position

	// Name is the element name.
	Name esixml.Name

	// Name is the expected end element name, if any.
	Expected esixml.Name
}

// Error returns a human-readable error message.
func (u *UnexpectedEndElementError) Error() string {
	if u.Expected.Local == "" {
		return fmt.Sprintf(`unexpected end element %s at position %s`, u.Name, u.Position)
	}

	return fmt.Sprintf(`unexpected end element %s at position %s, %s expected`, u.Name, u.Position, u.Expected)
}

// Is checks if the given error matches the receiver.
func (u *UnexpectedEndElementError) Is(err error) bool {
	var o *UnexpectedEndElementError
	return errors.As(err, &o) && *o == *u
}

// UnexpectedTokenError is returned when a specific token type was expected, but a different type was found.
type UnexpectedTokenError struct {
	Position Position

	// Type is the type of token that was encountered
	Type esixml.TokenType

	// Expected, if set, contains a list of expected token types.
	Expected []esixml.TokenType
}

// Error returns a human-readable error message.
func (u *UnexpectedTokenError) Error() string {
	return fmt.Sprintf(`unexpected token %s at position %s`, u.Type, u.Position)
}

// Is checks if the given error matches the receiver.
func (u *UnexpectedTokenError) Is(err error) bool {
	var o *UnexpectedTokenError
	return errors.As(err, &o) && o.Type == u.Type && slices.Equal(o.Expected, u.Expected)
}

// Node is the interface implemented by all ESI elements as well as [RawData].
type Node interface {
	// Pos returns the start and end position of the Node.
	Pos() (start, end int)

	node()
}

// Nodes is a simple slice of [Node] values.
type Nodes []Node

// Element is the interface implemented by all Node types that are based on ESI elements.
type Element interface {
	Node

	// Name returns the name of the element with the "esi" namespace.
	Name() esixml.Name
}

// Position is embedded into [Node] types and contains the start and end offsets of the node in the parsed input.
type Position = esixml.Position

// AttemptElement represents a <esi:attempt> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.3 try | attempt | except.
type AttemptElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Nodes contains all child nodes of the element.
	Nodes Nodes
}

var _ Element = (*AttemptElement)(nil)

func (*AttemptElement) node() {}

// Name returns the element name.
func (e *AttemptElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "attempt"}
}

// Pos returns the start and end position of the element.
func (e *AttemptElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// ChooseElement represents a <esi:choose> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.2 choose | when | otherwise.
type ChooseElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// When contains all <esi:when> nodes included in the element.
	When []*WhenElement

	// Otherwise contains the <esi:otherwise> element included in the element, if any.
	Otherwise *OtherwiseElement
}

var _ Element = (*ChooseElement)(nil)

func (*ChooseElement) node() {}

// Name returns the element name.
func (e *ChooseElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "choose"}
}

// Pos returns the start and end position of the element.
func (e *ChooseElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// CommentElement represents a <esi:comment> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.4 comment.
type CommentElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Text contains the comment text.
	Text string
}

var _ Element = (*CommentElement)(nil)

func (*CommentElement) node() {}

// Name returns the element name.
func (e *CommentElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "comment"}
}

// Pos returns the start and end position of the element.
func (e *CommentElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// ExceptElement represents a <esi:except> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.3 try | attempt | except.
type ExceptElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Nodes contains all child nodes of the element.
	Nodes Nodes
}

var _ Element = (*ExceptElement)(nil)

func (*ExceptElement) node() {}

// Name returns the element name.
func (e *ExceptElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "except"}
}

// Pos returns the start and end position of the element.
func (e *ExceptElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// IncludeElement represents a <esi:include> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.1 include.
type IncludeElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Alt contains the alternative source that should be included, if the normal source is unavailable.
	Alt string

	// OnError contains the specified behaviour for errors.
	OnError ErrorBehaviour

	// Source contains the source that should be included.
	Source string
}

var _ Element = (*IncludeElement)(nil)

func (*IncludeElement) node() {}

// Name returns the element name.
func (e *IncludeElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "include"}
}

// Pos returns the start and end position of the element.
func (e *IncludeElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// InlineElement represents a <esi:inline> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.2 inline.
type InlineElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Name contains the name of the fragment.
	FragmentName string

	// Fragment is true if the fragment can be fetched.
	Fetchable bool

	// Data contains the unprocessed content of the element.
	Data RawData
}

var _ Element = (*InlineElement)(nil)

func (*InlineElement) node() {}

// Name returns the element name.
func (e *InlineElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "inline"}
}

// Pos returns the start and end position of the element.
func (e *InlineElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// OtherwiseElement represents a <esi:otherwise> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.2 choose | when | otherwise.
type OtherwiseElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Nodes contains all child nodes of the element.
	Nodes Nodes
}

var _ Element = (*OtherwiseElement)(nil)

func (*OtherwiseElement) node() {}

// Name returns the element name.
func (e *OtherwiseElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "otherwise"}
}

// Pos returns the start and end position of the element.
func (e *OtherwiseElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// RawData represents raw, unprocessed data.
//
// The data may contain anything, including valid ESI elements.
type RawData struct {
	Position Position

	// Bytes contains the unprocessed data.
	Bytes []byte
}

func (*RawData) node() {}

// Pos returns the position.
func (r *RawData) Pos() (start, end int) {
	return r.Position.Pos()
}

// RemoveElement represents a <esi:remove> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.5 remove.
type RemoveElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Data contains the unprocessed data of the element.
	Data RawData
}

var _ Element = (*RemoveElement)(nil)

func (*RemoveElement) node() {}

// Name returns the element name.
func (e *RemoveElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "remove"}
}

// Pos returns the start and end position of the element.
func (e *RemoveElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// TryElement represents a <esi:try> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.3 try | attempt | except.
type TryElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Attempt contains the nodes that should be attempted to be rendered.
	Attempt *AttemptElement // attempt

	// Except contains the nodes that should be rendered if the initial attempt failed.
	Except *ExceptElement // except
}

var _ Element = (*TryElement)(nil)

func (*TryElement) node() {}

// Name returns the element name.
func (e *TryElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "try"}
}

// Pos returns the start and end position of the element.
func (e *TryElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// VarsElement represents a <esi:vars> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.6 vars.
type VarsElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Nodes contains all child nodes of the element.
	Nodes Nodes
}

var _ Element = (*VarsElement)(nil)

func (*VarsElement) node() {}

// Name returns the element name.
func (e *VarsElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "vars"}
}

// Pos returns the start and end position of the element.
func (e *VarsElement) Pos() (start, end int) {
	return e.Position.Pos()
}

// WhenElement represents a <esi:when> element.
//
// See https://www.w3.org/TR/esi-lang/, 3.2 choose | when | otherwise.
type WhenElement struct {
	Position Position

	// Attr contains all non-standard attributes specified on the element.
	Attr []esixml.Attr

	// Test contains the condition for the element.
	Test string

	// Nodes contains all child nodes of the element.
	Nodes Nodes
}

var _ Element = (*WhenElement)(nil)

func (*WhenElement) node() {}

// Name returns the element name.
func (e *WhenElement) Name() esixml.Name {
	return esixml.Name{Space: "esi", Local: "when"}
}

// Pos returns the start and end position of the element.
func (e *WhenElement) Pos() (start, end int) {
	return e.Position.Pos()
}

type parser struct {
	data []byte

	reader      esixml.Reader
	unreadToken esixml.Token

	nodes []Nodes

	stateFn func(*parser) error
}

var parserPool = sync.Pool{
	New: func() any {
		return &parser{}
	},
}

func getParser(data []byte) *parser {
	p, _ := parserPool.Get().(*parser)
	p.reset(data)
	return p
}

func putParser(p *parser) {
	p.reset(nil)
	parserPool.Put(p)
}

// Parse parses the given []byte and returns all ESI elements as well as any non-ESI data.
func Parse(data []byte) (Nodes, error) {
	p := getParser(data)
	defer putParser(p)

	for {
		if err := p.stateFn(p); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}

			return nil, err
		}
	}

	if parent := p.currentParent(); parent != nil {
		start, end := parent.Pos()

		return nil, &UnclosedElementError{
			Position: Position{
				Start: start,
				End:   end,
			},
			Name: parent.(Element).Name(),
		}
	}

	return p.nodes[0], nil
}

func (p *parser) next() (esixml.Token, error) {
	if p.unreadToken.Type != esixml.TokenTypeInvalid {
		t := p.unreadToken
		p.unreadToken = esixml.Token{}
		return t, nil
	}

	return p.reader.Next()
}

func (p *parser) mustNext() (esixml.Token, error) {
	tok, err := p.next()
	if err != nil {
		if errors.Is(err, io.EOF) {
			err = io.ErrUnexpectedEOF
		}
		return esixml.Token{}, err
	}
	return tok, nil
}

func (p *parser) mustNextTyped(typ esixml.TokenType) (esixml.Token, error) {
	tok, err := p.mustNext()
	if err != nil {
		return esixml.Token{}, err
	}
	if tok.Type != typ {
		return esixml.Token{}, &UnexpectedTokenError{
			Position: tok.Position,
			Type:     tok.Type,
			Expected: []esixml.TokenType{typ},
		}
	}
	return tok, nil
}

func (p *parser) mustNextEndElement(localName string) (esixml.Token, error) {
	tok, err := p.mustNextTyped(esixml.TokenTypeEndElement)
	if err != nil {
		return esixml.Token{}, err
	}
	if tok.Name.Local != localName {
		return esixml.Token{}, &UnexpectedElementError{
			Position: tok.Position,
			Name:     tok.Name,
		}
	}
	return tok, nil
}

func (p *parser) mustNextStartElement(localName string) (esixml.Token, error) {
	tok, err := p.mustNextTyped(esixml.TokenTypeStartElement)
	if err != nil {
		return esixml.Token{}, err
	}
	if tok.Name.Local != localName {
		return esixml.Token{}, &UnexpectedElementError{
			Position: tok.Position,
			Name:     tok.Name,
		}
	}
	return tok, nil
}

func (p *parser) currentParent() Node {
	level := len(p.nodes) - 2
	if level < 0 || len(p.nodes[level]) == 0 {
		return nil
	}
	return p.nodes[level][len(p.nodes[level])-1]
}

func (p *parser) push(node Node) {
	level := len(p.nodes) - 1
	if p.nodes[level] == nil {
		p.nodes[level] = make(Nodes, 0, 32)
	}
	p.nodes[level] = append(p.nodes[level], node)
}

func (p *parser) pushAndEnter(node Element) {
	level := len(p.nodes) - 1
	p.nodes[level] = append(p.nodes[level], node)
	p.nodes = append(p.nodes, make(Nodes, 0, 4))
}

func (p *parser) exit() Nodes {
	nodes := p.nodes[len(p.nodes)-1]
	p.nodes = p.nodes[:len(p.nodes)-1]
	return nodes
}

func takeAttr(attrs *[]esixml.Attr, name string) (esixml.Attr, bool) {
	for i, attr := range *attrs {
		if attr.Name.Space != "" || attr.Name.Local != name {
			continue
		}

		// Do not keep the backing array if not needed anymore
		if len(*attrs) == 1 {
			*attrs = nil
		} else {
			*attrs = slices.Delete(*attrs, i, i+1)
		}

		return attr, true
	}
	return esixml.Attr{}, false
}

func (p *parser) parseAttemptElement() error {
	tok, err := p.mustNextStartElement("attempt")
	if err != nil {
		return err
	}

	if _, ok := p.currentParent().(*TryElement); !ok {
		return &UnexpectedElementError{Position: tok.Position, Name: tok.Name}
	}

	p.pushAndEnter(&AttemptElement{Position: tok.Position, Attr: tok.Attr})
	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseAttemptElementEnd() error {
	tok, err := p.mustNextEndElement("attempt")
	if err != nil {
		return err
	}

	parent, _ := p.currentParent().(*AttemptElement)
	parent.Position.End = tok.Position.End
	parent.Nodes = p.exit()

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseChooseElement() error {
	tok, err := p.mustNextStartElement("choose")
	if err != nil {
		return err
	}

	if tok.Closed {
		return &EmptyElementError{Position: tok.Position, Name: tok.Name}
	}

	p.pushAndEnter(&ChooseElement{Position: tok.Position, Attr: tok.Attr})
	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseChooseElementEnd() error {
	tok, err := p.mustNextEndElement("choose")
	if err != nil {
		return err
	}

	parent, _ := p.currentParent().(*ChooseElement)
	parent.Position.End = tok.Position.End

	nodes := p.exit()

	for _, node := range nodes {
		switch v := node.(type) {
		case *WhenElement:
			parent.When = append(parent.When, v)
		case *OtherwiseElement:
			if parent.Otherwise != nil {
				return &DuplicateElementError{Position: v.Position, Name: v.Name()}
			}
			parent.Otherwise = v
		default:
			// ignore other data, as per spec
		}
	}

	if len(parent.When) == 0 {
		return &MissingElementError{Position: tok.Position, Name: esixml.Name{Space: "esi", Local: "when"}}
	}

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseCommentElement() error {
	tok, err := p.mustNextStartElement("comment")
	if err != nil {
		return err
	}

	if !tok.Closed {
		return &UnclosedElementError{Position: tok.Position, Name: tok.Name}
	}

	text, ok := takeAttr(&tok.Attr, "text")
	if !ok {
		return &MissingAttributeError{Position: tok.Position, Element: tok.Name, Attribute: esixml.Name{Local: "text"}}
	}

	p.push(&CommentElement{
		Position: tok.Position,
		Attr:     tok.Attr,
		Text:     text.Value,
	})

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseData() error {
	tok, err := p.mustNextTyped(esixml.TokenTypeData)
	if err != nil {
		return err
	}

	p.push(&RawData{Position: tok.Position, Bytes: tok.Data})
	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseDataOrElement() error {
	tok, err := p.next()
	if err != nil {
		return err
	}

	switch tok.Type {
	case esixml.TokenTypeInvalid:
		panic("unreachable")
	case esixml.TokenTypeStartElement:
		p.stateFn = (*parser).parseElement
	case esixml.TokenTypeEndElement:
		p.stateFn = (*parser).parseEndElement
	case esixml.TokenTypeData:
		p.stateFn = (*parser).parseData
	}

	p.unreadToken = tok
	return nil
}

func (p *parser) parseElement() error {
	tok, err := p.mustNextTyped(esixml.TokenTypeStartElement)
	if err != nil {
		return err
	}

	switch tok.Name.Local {
	case "attempt":
		p.stateFn = (*parser).parseAttemptElement
	case "choose":
		p.stateFn = (*parser).parseChooseElement
	case "comment":
		p.stateFn = (*parser).parseCommentElement
	case "except":
		p.stateFn = (*parser).parseExceptElement
	case "include":
		p.stateFn = (*parser).parseIncludeElement
	case "inline":
		p.stateFn = (*parser).parseInlineElement
	case "otherwise":
		p.stateFn = (*parser).parseOtherwiseElement
	case "remove":
		p.stateFn = (*parser).parseRemoveElement
	case "try":
		p.stateFn = (*parser).parseTryElement
	case "vars":
		p.stateFn = (*parser).parseVarsElement
	case "when":
		p.stateFn = (*parser).parseWhenElement
	default:
		return &InvalidElementError{Position: tok.Position, Name: tok.Name}
	}

	p.unreadToken = tok
	return nil
}

func (p *parser) parseEndElement() error {
	tok, err := p.mustNextTyped(esixml.TokenTypeEndElement)
	if err != nil {
		return err
	}

	parent, _ := p.currentParent().(Element)
	if parent == nil {
		return &UnexpectedEndElementError{Position: tok.Position, Name: tok.Name}
	}
	if parent.Name() != tok.Name {
		return &UnexpectedEndElementError{Position: tok.Position, Name: tok.Name, Expected: parent.Name()}
	}

	switch tok.Name.Local {
	case "attempt":
		p.stateFn = (*parser).parseAttemptElementEnd
	case "choose":
		p.stateFn = (*parser).parseChooseElementEnd
	case "except":
		p.stateFn = (*parser).parseExceptElementEnd
	case "otherwise":
		p.stateFn = (*parser).parseOtherwiseElementEnd
	case "try":
		p.stateFn = (*parser).parseTryElementEnd
	case "vars":
		p.stateFn = (*parser).parseVarsElementEnd
	case "when":
		p.stateFn = (*parser).parseWhenElementEnd
	default:
		return &InvalidElementError{Position: tok.Position, Name: tok.Name}
	}

	p.unreadToken = tok
	return nil
}

func (p *parser) parseExceptElement() error {
	tok, err := p.mustNextStartElement("except")
	if err != nil {
		return err
	}

	if _, ok := p.currentParent().(*TryElement); !ok {
		return &UnexpectedElementError{Position: tok.Position, Name: tok.Name}
	}

	p.pushAndEnter(&ExceptElement{Position: tok.Position, Attr: tok.Attr})
	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseExceptElementEnd() error {
	tok, err := p.mustNextEndElement("except")
	if err != nil {
		return err
	}

	parent, _ := p.currentParent().(*ExceptElement)
	parent.Position.End = tok.Position.End
	parent.Nodes = p.exit()

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseIncludeElement() error {
	tok, err := p.mustNextStartElement("include")
	if err != nil {
		return err
	}

	if !tok.Closed {
		return &UnclosedElementError{Position: tok.Position, Name: tok.Name}
	}

	alt, _ := takeAttr(&tok.Attr, "alt")

	onError, ok := takeAttr(&tok.Attr, "onerror")
	if ok && onError.Value != string(ErrorBehaviourContinue) {
		return &InvalidAttributeValueError{
			Position: Position{Start: onError.Position.Start, End: onError.Position.End},
			Element:  tok.Name,
			Name:     esixml.Name{Local: "onerror"},
			Value:    onError.Value,
			Allowed: []string{
				string(ErrorBehaviourContinue),
			},
		}
	}

	src, ok := takeAttr(&tok.Attr, "src")
	if !ok {
		return &MissingAttributeError{Position: tok.Position, Element: tok.Name, Attribute: esixml.Name{Local: "src"}}
	}

	p.push(&IncludeElement{
		Position: tok.Position,
		Attr:     tok.Attr,
		Alt:      alt.Value,
		OnError:  ErrorBehaviour(onError.Value),
		Source:   src.Value,
	})

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseInlineElement() error {
	tok, err := p.mustNextStartElement("inline")
	if err != nil {
		return err
	}

	name, ok := takeAttr(&tok.Attr, "name")
	if !ok {
		return &MissingAttributeError{Position: tok.Position, Element: tok.Name, Attribute: esixml.Name{Local: "name"}}
	}

	fetchable, ok := takeAttr(&tok.Attr, "fetchable")
	if !ok {
		return &MissingAttributeError{Position: tok.Position, Element: tok.Name, Attribute: esixml.Name{Local: "fetchable"}}
	}

	switch fetchable.Value {
	case "no", "yes":
	default:
		return &InvalidAttributeValueError{
			Position: tok.Position,
			Element:  tok.Name,
			Name:     esixml.Name{Local: "fetchable"},
			Value:    fetchable.Value,
			Allowed:  []string{"no", "yes"},
		}
	}

	if tok.Closed {
		return &EmptyElementError{Position: tok.Position, Name: tok.Name}
	}

	e := &InlineElement{
		Position:     tok.Position,
		Attr:         tok.Attr,
		FragmentName: name.Value,
		Fetchable:    fetchable.Value == "yes",
	}
	e.Data.Position.Start = tok.Position.End

	p.pushAndEnter(e)

	for {
		tok, err := p.next()
		if err != nil {
			return err
		}

		if tok.Type != esixml.TokenTypeEndElement || tok.Name.Local != "inline" {
			continue
		}

		e.Data.Bytes = slices.Clone(p.data[e.Data.Position.Start:tok.Position.Start])
		e.Data.Position.End = tok.Position.Start
		e.Position.End = tok.Position.End
		p.exit()
		break
	}

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseOtherwiseElement() error {
	tok, err := p.mustNextStartElement("otherwise")
	if err != nil {
		return err
	}

	if _, ok := p.currentParent().(*ChooseElement); !ok {
		return &UnexpectedElementError{Position: tok.Position, Name: tok.Name}
	}

	p.pushAndEnter(&OtherwiseElement{Position: tok.Position, Attr: tok.Attr})
	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseOtherwiseElementEnd() error {
	tok, err := p.mustNextEndElement("otherwise")
	if err != nil {
		return err
	}

	parent, _ := p.currentParent().(*OtherwiseElement)
	parent.Position.End = tok.Position.End
	parent.Nodes = p.exit()

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseRemoveElement() error {
	tok, err := p.mustNextStartElement("remove")
	if err != nil {
		return err
	}

	if tok.Closed {
		return &EmptyElementError{Position: tok.Position, Name: tok.Name}
	}

	e := &RemoveElement{Position: tok.Position, Attr: tok.Attr}
	e.Data.Position.Start = tok.Position.End

	p.pushAndEnter(e)

	for {
		tok, err := p.next()
		if err != nil {
			return err
		}

		if tok.Type != esixml.TokenTypeEndElement || tok.Name.Local != "remove" {
			continue
		}

		e.Data.Bytes = slices.Clone(p.data[e.Data.Position.Start:tok.Position.Start])
		e.Data.Position.End = tok.Position.Start
		e.Position.End = tok.Position.End
		p.exit()
		break
	}

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseTryElement() error {
	tok, err := p.mustNextStartElement("try")
	if err != nil {
		return err
	}

	if tok.Closed {
		return &EmptyElementError{Position: tok.Position, Name: tok.Name}
	}

	p.pushAndEnter(&TryElement{Position: tok.Position, Attr: tok.Attr})
	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseTryElementEnd() error {
	tok, err := p.mustNextEndElement("try")
	if err != nil {
		return err
	}

	parent, _ := p.currentParent().(*TryElement)
	parent.Position.End = tok.Position.End

	nodes := p.exit()

	for _, node := range nodes {
		switch v := node.(type) {
		case *AttemptElement:
			if parent.Attempt != nil {
				return &DuplicateElementError{Position: v.Position, Name: v.Name()}
			}
			parent.Attempt = v
		case *ExceptElement:
			if parent.Except != nil {
				return &DuplicateElementError{Position: v.Position, Name: v.Name()}
			}
			parent.Except = v
		default:
			// ignore other data, as per spec
		}
	}

	if parent.Attempt == nil {
		return &MissingElementError{Position: tok.Position, Name: esixml.Name{Space: "esi", Local: "attempt"}}
	}

	if parent.Except == nil {
		return &MissingElementError{Position: tok.Position, Name: esixml.Name{Space: "esi", Local: "except"}}
	}

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseVarsElement() error {
	tok, err := p.mustNextStartElement("vars")
	if err != nil {
		return err
	}

	if tok.Closed {
		return &EmptyElementError{Position: tok.Position, Name: tok.Name}
	}

	e := &VarsElement{Position: tok.Position, Attr: tok.Attr}

	p.pushAndEnter(e)
	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseVarsElementEnd() error {
	tok, err := p.mustNextEndElement("vars")
	if err != nil {
		return err
	}

	parent, _ := p.currentParent().(*VarsElement)
	parent.Position.End = tok.Position.End
	parent.Nodes = p.exit()

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseWhenElement() error {
	tok, err := p.mustNextStartElement("when")
	if err != nil {
		return err
	}

	if _, ok := p.currentParent().(*ChooseElement); !ok {
		return &UnexpectedElementError{Position: tok.Position, Name: tok.Name}
	}

	test, ok := takeAttr(&tok.Attr, "test")
	if !ok {
		return &MissingAttributeError{Position: tok.Position, Element: tok.Name, Attribute: esixml.Name{Local: "test"}}
	}

	p.pushAndEnter(&WhenElement{Position: tok.Position, Attr: tok.Attr, Test: test.Value})
	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) parseWhenElementEnd() error {
	tok, err := p.mustNextEndElement("when")
	if err != nil {
		return err
	}

	parent, _ := p.currentParent().(*WhenElement)
	parent.Position.End = tok.Position.End
	parent.Nodes = p.exit()

	p.stateFn = (*parser).parseDataOrElement
	return nil
}

func (p *parser) reset(data []byte) {
	if len(p.nodes) == 0 || cap(p.nodes) > 4 {
		p.nodes = make([]Nodes, 1, 4)
	} else {
		clear(p.nodes)
	}

	p.data = data
	p.nodes = p.nodes[:1]
	p.unreadToken = esixml.Token{}
	p.stateFn = (*parser).parseDataOrElement
	p.reader.Reset(data)
}
