// Package esixml implements a simple XML parser for ESI documents, that only parses ESI elements.
package esixml

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"strconv"
	"unicode"
	"unicode/utf8"
)

var stringsMap = map[string]string{
	"alt":       "alt",
	"attempt":   "attempt",
	"choose":    "choose",
	"comment":   "comment",
	"continue":  "continue",
	"esi":       "esi",
	"except":    "except",
	"fetchable": "fetchable",
	"include":   "include",
	"inline":    "inline",
	"name":      "name",
	"onerror":   "onerror",
	"otherwise": "otherwise",
	"remove":    "remove",
	"src":       "src",
	"test":      "test",
	"text":      "text",
	"try":       "try",
	"vars":      "vars",
	"when":      "when",
}

func bytesToName(b []byte) Name {
	ns, name, ok := bytes.Cut(b, []byte(":"))
	if !ok {
		return Name{Local: bytesToString(b)}
	}
	return Name{Space: bytesToString(ns), Local: bytesToString(name)}
}

func bytesToString(b []byte) string {
	s, ok := stringsMap[string(b)]
	if !ok {
		return string(b)
	}
	return s
}

// DuplicateAttributeError is returned when encountering an ESI element with duplicate attributes.
type DuplicateAttributeError struct {
	// Offset is the position in the input where the error occurred.
	Offset int

	// Name is the name of the duplicated attributes.
	Name string
}

// Error returns a human-readable error message.
func (d *DuplicateAttributeError) Error() string {
	return fmt.Sprintf("duplicate attribute %q at offset %d", d.Name, d.Offset)
}

// Is checks if the given error matches the receiver.
func (d *DuplicateAttributeError) Is(err error) bool {
	var o *DuplicateAttributeError
	return errors.As(err, &o) && *o == *d
}

// SyntaxError is returned when encountering invalid XML when processing ESI elements.
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

// Is checks if the given error matches the receiver.
func (s *SyntaxError) Is(err error) bool {
	var o *SyntaxError
	return errors.As(err, &o) && o.Offset == s.Offset && o.Message == s.Message
}

// Unwrap returns e.Underlying.
func (s *SyntaxError) Unwrap() error {
	return s.Underlying
}

// UnsupportedEntityError is returned when encountering a non-standard named entity inside an attribute.
type UnsupportedEntityError struct {
	// Offset is the position in the input where the error occurred.
	Offset int
}

// Error returns a human-readable error message.
func (u *UnsupportedEntityError) Error() string {
	return fmt.Sprintf("unsupported XML entity at offset %d", u.Offset)
}

// Is checks if the given error matches the receiver.
func (u *UnsupportedEntityError) Is(err error) bool {
	var o *UnsupportedEntityError
	return errors.As(err, &o) && *o == *u
}

type Attr struct {
	// Position contains the position of the attribute in the input.
	Position Position

	// Name contains the attribute name.
	Name Name

	// Value contains the unescaped attribute value.
	Value string
}

// Name is a copy of Name, so that we do not have to depend on encoding/xml.
type Name struct {
	// Space is the part of the name before the colon, if any.
	Space string

	// Local is the part of the name after the colon or the whole name if there is no colon.
	Local string
}

// String implements the [fmt.GoStringer] interface.
func (n Name) String() string {
	if n.Space == "" {
		return n.Local
	}
	return n.Space + ":" + n.Local
}

// Position specifies a start and end position in a parsed []byte.
type Position struct {
	// Start is the inclusive start index.
	Start int

	// End is the exclusive end index.
	End int
}

// Pos returns the start and end position of the [Node].
func (p Position) Pos() (start, end int) {
	return p.Start, p.End
}

// String implements the [fmt.GoStringer] interface.
func (p Position) String() string {
	return strconv.Itoa(p.Start) + ":" + strconv.Itoa(p.End)
}

// Token represents a parsed "token" returned by a [Reader].
type Token struct {
	// Position contains the position of the token in the input.
	Position Position

	// Type describes the type of the token.
	Type TokenType

	// Name is the name of the XML element if Type is either [TokenTypeElementStart] or [TokenTypeElementEnd].
	Name Name

	// Attr are the attributes of the XML element if Type is [TokenTypeElementStart].
	Attr []Attr

	// Data contains the raw data if Type is [TokenTypeData].
	//
	// The data may be anything, including valid XML.
	Data []byte

	// Closed is true if the token type is [TokenTypeStartElement] and the element was self-closing.
	Closed bool
}

func (t *Token) hasAttr(name Name) bool {
	for _, attr := range t.Attr {
		if attr.Name == name {
			return true
		}
	}

	return false
}

// TokenType is an enum of the possible types of tokens.
type TokenType uint8

const (
	// TokenTypeInvalid is the zero value for TokenType and is not a valid type.
	TokenTypeInvalid TokenType = iota

	// TokenTypeStartElement indicates that a [Token] represents a starting ESI element, e.g. "<esi:include".
	TokenTypeStartElement

	// TokenTypeEndElement indicates that a [Token] represents the end of a previously started element.
	TokenTypeEndElement

	// TokenTypeData indicates that a [Token] contains raw, unprocessed data.
	TokenTypeData
)

// String returns the name of the type.
func (t TokenType) String() string {
	switch t {
	case TokenTypeInvalid:
		return "TokenTypeInvalid"
	case TokenTypeStartElement:
		return "TokenTypeStartElement"
	case TokenTypeEndElement:
		return "TokenTypeEndElement"
	case TokenTypeData:
		return "TokenTypeData"
	default:
		panic("unknown token type")
	}
}

// Reader allows reading ESI tags and attributes from a []byte.
//
// It only looks for opening and closing ESI tags and simply returns all other data unprocessed.
type Reader struct {
	data    []byte
	offset  int
	err     error
	scratch [32]byte

	stateFn func(*Reader) (Token, error)
}

// NewReader returns a new Reader set to use the given input.
//
// This is the same as calling [Reader.Reset] on an existing reader.
func NewReader(data []byte) *Reader {
	r := &Reader{}
	r.Reset(data)
	return r
}

// All returns all remaining tokens from the reader.
func (r *Reader) All(yield func(Token, error) bool) {
	for {
		t, err := r.Next()

		if errors.Is(err, io.EOF) {
			return
		}

		if !yield(t, err) {
			return
		}

		if err != nil {
			return
		}
	}
}

// Offset returns the byte offset in the []byte from which data is read.
func (r *Reader) Offset() int {
	return r.offset
}

// Next returns the next token if any.
//
// If an error occurred, future calls will return the same error.
//
// After all data was read, if there were no previous errors, Next will return [io.EOF].
func (r *Reader) Next() (Token, error) {
	for {
		if r.err != nil {
			return Token{}, r.err
		}

		token, err := r.stateFn(r)
		if err != nil {
			r.err = err
			return Token{}, err
		}

		// Ignore empty data tokens. This makes the parsing logic simpler
		if token.Type == TokenTypeData && len(token.Data) == 0 {
			continue
		}

		return token, nil
	}
}

// Reset resets the Reader to read from the given []byte.
//
// This allows re-using the reader and can also be used to initialize a reader without using [NewReader].
func (r *Reader) Reset(data []byte) {
	clear(r.scratch[:])

	r.data = data
	r.offset = 0
	r.err = nil
	r.stateFn = (*Reader).parseElementOrData
}

func (r *Reader) consume(b byte) bool {
	if r.offset >= len(r.data) || r.data[r.offset] != b {
		return false
	}
	r.offset++
	return true
}

func (r *Reader) discardSpaces() {
	for r.offset < len(r.data) {
		switch r.data[r.offset] {
		case ' ', '\r', '\n', '\t':
			r.offset++
		default:
			return
		}
	}
}

func (r *Reader) peek() byte {
	if r.offset >= len(r.data) {
		return 0
	}
	return r.data[r.offset]
}

func (r *Reader) parseEndElement() (Token, error) {
	t := Token{Type: TokenTypeEndElement, Position: Position{Start: r.offset}}

	_ = r.consume('<')
	_ = r.consume('/')

	var err error

	if t.Name, err = r.readName(false); err != nil {
		return Token{}, err
	}

	r.discardSpaces()

	if !r.consume('>') {
		return Token{}, &SyntaxError{Offset: r.offset, Message: "expected '>'"}
	}

	t.Position.End = r.offset

	r.stateFn = (*Reader).parseElementOrData
	return t, nil
}

func (r *Reader) parseElementOrData() (Token, error) {
	offset := r.offset

	for {
		index := bytes.IndexByte(r.data[offset:], '<')
		if index == -1 {
			// We reached the end of the data with no element, so take the rest as unprocessed data. If there is none,
			t := Token{Type: TokenTypeData, Position: Position{Start: r.offset, End: len(r.data)}, Data: r.data[r.offset:]}

			r.offset = len(r.data)
			r.err = io.EOF

			return t, nil
		}

		offset += index

		switch {
		case bytes.HasPrefix(r.data[offset:], []byte("<esi:")):
			// If this is empty, it will be ignored
			t := Token{Type: TokenTypeData, Position: Position{Start: r.offset, End: offset}, Data: r.data[r.offset:offset]}

			r.offset = offset
			r.stateFn = (*Reader).parseStartElement

			return t, nil
		case bytes.HasPrefix(r.data[offset:], []byte("</esi:")):
			// If this is empty, it will be ignored
			t := Token{Type: TokenTypeData, Position: Position{Start: r.offset, End: offset}, Data: r.data[r.offset:offset]}

			r.offset = offset
			r.stateFn = (*Reader).parseEndElement

			return t, nil
		default:
			// Not an esi tag, so just continue from after the "<"
			offset++
		}
	}
}

func (r *Reader) parseStartElement() (Token, error) {
	t := Token{Type: TokenTypeStartElement, Position: Position{Start: r.offset}}

	_ = r.consume('<')

	var err error

	if t.Name, err = r.readName(false); err != nil {
		return Token{}, err
	}

	for {
		r.discardSpaces()

		if r.consume('/') {
			t.Closed = true

			if !r.consume('>') {
				r.err = &SyntaxError{Offset: r.offset, Message: "expected '>'"}
				return Token{}, r.err
			}

			break
		}

		if r.consume('>') {
			break
		}

		offset := r.offset

		attrName, err := r.readName(false)
		if err != nil {
			return Token{}, err
		}

		r.discardSpaces()

		b, err := r.readByte()
		if err != nil {
			return Token{}, err
		}

		if b != '=' {
			r.offset--
			return Token{}, &SyntaxError{Offset: r.offset, Message: "expected '='"}
		}

		attrValue, err := r.readAttrValue()
		if err != nil {
			return Token{}, err
		}

		if t.Attr == nil {
			t.Attr = make([]Attr, 0, 4)
		}

		if t.hasAttr(attrName) {
			return Token{}, &DuplicateAttributeError{Offset: offset, Name: attrName.Local}
		}

		t.Attr = append(t.Attr, Attr{
			Position: Position{Start: offset, End: r.offset},
			Name:     attrName,
			Value:    attrValue,
		})
	}

	t.Position.End = r.offset

	r.stateFn = (*Reader).parseElementOrData
	return t, nil
}

// From https://github.com/golang/go/blob/7a2689b152785010ee2013fb220a048bfe31e49f/src/encoding/xml/xml.go#L1289-L1482
var first = &unicode.RangeTable{
	R16: []unicode.Range16{
		{0x003A, 0x003A, 1},
		{0x0041, 0x005A, 1},
		{0x005F, 0x005F, 1},
		{0x0061, 0x007A, 1},
		{0x00C0, 0x00D6, 1},
		{0x00D8, 0x00F6, 1},
		{0x00F8, 0x00FF, 1},
		{0x0100, 0x0131, 1},
		{0x0134, 0x013E, 1},
		{0x0141, 0x0148, 1},
		{0x014A, 0x017E, 1},
		{0x0180, 0x01C3, 1},
		{0x01CD, 0x01F0, 1},
		{0x01F4, 0x01F5, 1},
		{0x01FA, 0x0217, 1},
		{0x0250, 0x02A8, 1},
		{0x02BB, 0x02C1, 1},
		{0x0386, 0x0386, 1},
		{0x0388, 0x038A, 1},
		{0x038C, 0x038C, 1},
		{0x038E, 0x03A1, 1},
		{0x03A3, 0x03CE, 1},
		{0x03D0, 0x03D6, 1},
		{0x03DA, 0x03E0, 2},
		{0x03E2, 0x03F3, 1},
		{0x0401, 0x040C, 1},
		{0x040E, 0x044F, 1},
		{0x0451, 0x045C, 1},
		{0x045E, 0x0481, 1},
		{0x0490, 0x04C4, 1},
		{0x04C7, 0x04C8, 1},
		{0x04CB, 0x04CC, 1},
		{0x04D0, 0x04EB, 1},
		{0x04EE, 0x04F5, 1},
		{0x04F8, 0x04F9, 1},
		{0x0531, 0x0556, 1},
		{0x0559, 0x0559, 1},
		{0x0561, 0x0586, 1},
		{0x05D0, 0x05EA, 1},
		{0x05F0, 0x05F2, 1},
		{0x0621, 0x063A, 1},
		{0x0641, 0x064A, 1},
		{0x0671, 0x06B7, 1},
		{0x06BA, 0x06BE, 1},
		{0x06C0, 0x06CE, 1},
		{0x06D0, 0x06D3, 1},
		{0x06D5, 0x06D5, 1},
		{0x06E5, 0x06E6, 1},
		{0x0905, 0x0939, 1},
		{0x093D, 0x093D, 1},
		{0x0958, 0x0961, 1},
		{0x0985, 0x098C, 1},
		{0x098F, 0x0990, 1},
		{0x0993, 0x09A8, 1},
		{0x09AA, 0x09B0, 1},
		{0x09B2, 0x09B2, 1},
		{0x09B6, 0x09B9, 1},
		{0x09DC, 0x09DD, 1},
		{0x09DF, 0x09E1, 1},
		{0x09F0, 0x09F1, 1},
		{0x0A05, 0x0A0A, 1},
		{0x0A0F, 0x0A10, 1},
		{0x0A13, 0x0A28, 1},
		{0x0A2A, 0x0A30, 1},
		{0x0A32, 0x0A33, 1},
		{0x0A35, 0x0A36, 1},
		{0x0A38, 0x0A39, 1},
		{0x0A59, 0x0A5C, 1},
		{0x0A5E, 0x0A5E, 1},
		{0x0A72, 0x0A74, 1},
		{0x0A85, 0x0A8B, 1},
		{0x0A8D, 0x0A8D, 1},
		{0x0A8F, 0x0A91, 1},
		{0x0A93, 0x0AA8, 1},
		{0x0AAA, 0x0AB0, 1},
		{0x0AB2, 0x0AB3, 1},
		{0x0AB5, 0x0AB9, 1},
		{0x0ABD, 0x0AE0, 0x23},
		{0x0B05, 0x0B0C, 1},
		{0x0B0F, 0x0B10, 1},
		{0x0B13, 0x0B28, 1},
		{0x0B2A, 0x0B30, 1},
		{0x0B32, 0x0B33, 1},
		{0x0B36, 0x0B39, 1},
		{0x0B3D, 0x0B3D, 1},
		{0x0B5C, 0x0B5D, 1},
		{0x0B5F, 0x0B61, 1},
		{0x0B85, 0x0B8A, 1},
		{0x0B8E, 0x0B90, 1},
		{0x0B92, 0x0B95, 1},
		{0x0B99, 0x0B9A, 1},
		{0x0B9C, 0x0B9C, 1},
		{0x0B9E, 0x0B9F, 1},
		{0x0BA3, 0x0BA4, 1},
		{0x0BA8, 0x0BAA, 1},
		{0x0BAE, 0x0BB5, 1},
		{0x0BB7, 0x0BB9, 1},
		{0x0C05, 0x0C0C, 1},
		{0x0C0E, 0x0C10, 1},
		{0x0C12, 0x0C28, 1},
		{0x0C2A, 0x0C33, 1},
		{0x0C35, 0x0C39, 1},
		{0x0C60, 0x0C61, 1},
		{0x0C85, 0x0C8C, 1},
		{0x0C8E, 0x0C90, 1},
		{0x0C92, 0x0CA8, 1},
		{0x0CAA, 0x0CB3, 1},
		{0x0CB5, 0x0CB9, 1},
		{0x0CDE, 0x0CDE, 1},
		{0x0CE0, 0x0CE1, 1},
		{0x0D05, 0x0D0C, 1},
		{0x0D0E, 0x0D10, 1},
		{0x0D12, 0x0D28, 1},
		{0x0D2A, 0x0D39, 1},
		{0x0D60, 0x0D61, 1},
		{0x0E01, 0x0E2E, 1},
		{0x0E30, 0x0E30, 1},
		{0x0E32, 0x0E33, 1},
		{0x0E40, 0x0E45, 1},
		{0x0E81, 0x0E82, 1},
		{0x0E84, 0x0E84, 1},
		{0x0E87, 0x0E88, 1},
		{0x0E8A, 0x0E8D, 3},
		{0x0E94, 0x0E97, 1},
		{0x0E99, 0x0E9F, 1},
		{0x0EA1, 0x0EA3, 1},
		{0x0EA5, 0x0EA7, 2},
		{0x0EAA, 0x0EAB, 1},
		{0x0EAD, 0x0EAE, 1},
		{0x0EB0, 0x0EB0, 1},
		{0x0EB2, 0x0EB3, 1},
		{0x0EBD, 0x0EBD, 1},
		{0x0EC0, 0x0EC4, 1},
		{0x0F40, 0x0F47, 1},
		{0x0F49, 0x0F69, 1},
		{0x10A0, 0x10C5, 1},
		{0x10D0, 0x10F6, 1},
		{0x1100, 0x1100, 1},
		{0x1102, 0x1103, 1},
		{0x1105, 0x1107, 1},
		{0x1109, 0x1109, 1},
		{0x110B, 0x110C, 1},
		{0x110E, 0x1112, 1},
		{0x113C, 0x1140, 2},
		{0x114C, 0x1150, 2},
		{0x1154, 0x1155, 1},
		{0x1159, 0x1159, 1},
		{0x115F, 0x1161, 1},
		{0x1163, 0x1169, 2},
		{0x116D, 0x116E, 1},
		{0x1172, 0x1173, 1},
		{0x1175, 0x119E, 0x119E - 0x1175},
		{0x11A8, 0x11AB, 0x11AB - 0x11A8},
		{0x11AE, 0x11AF, 1},
		{0x11B7, 0x11B8, 1},
		{0x11BA, 0x11BA, 1},
		{0x11BC, 0x11C2, 1},
		{0x11EB, 0x11F0, 0x11F0 - 0x11EB},
		{0x11F9, 0x11F9, 1},
		{0x1E00, 0x1E9B, 1},
		{0x1EA0, 0x1EF9, 1},
		{0x1F00, 0x1F15, 1},
		{0x1F18, 0x1F1D, 1},
		{0x1F20, 0x1F45, 1},
		{0x1F48, 0x1F4D, 1},
		{0x1F50, 0x1F57, 1},
		{0x1F59, 0x1F5B, 0x1F5B - 0x1F59},
		{0x1F5D, 0x1F5D, 1},
		{0x1F5F, 0x1F7D, 1},
		{0x1F80, 0x1FB4, 1},
		{0x1FB6, 0x1FBC, 1},
		{0x1FBE, 0x1FBE, 1},
		{0x1FC2, 0x1FC4, 1},
		{0x1FC6, 0x1FCC, 1},
		{0x1FD0, 0x1FD3, 1},
		{0x1FD6, 0x1FDB, 1},
		{0x1FE0, 0x1FEC, 1},
		{0x1FF2, 0x1FF4, 1},
		{0x1FF6, 0x1FFC, 1},
		{0x2126, 0x2126, 1},
		{0x212A, 0x212B, 1},
		{0x212E, 0x212E, 1},
		{0x2180, 0x2182, 1},
		{0x3007, 0x3007, 1},
		{0x3021, 0x3029, 1},
		{0x3041, 0x3094, 1},
		{0x30A1, 0x30FA, 1},
		{0x3105, 0x312C, 1},
		{0x4E00, 0x9FA5, 1},
		{0xAC00, 0xD7A3, 1},
	},
}

// From https://github.com/golang/go/blob/7a2689b152785010ee2013fb220a048bfe31e49f/src/encoding/xml/xml.go#L1484-L1599
var second = &unicode.RangeTable{
	R16: []unicode.Range16{
		{0x002D, 0x002E, 1},
		{0x0030, 0x0039, 1},
		{0x00B7, 0x00B7, 1},
		{0x02D0, 0x02D1, 1},
		{0x0300, 0x0345, 1},
		{0x0360, 0x0361, 1},
		{0x0387, 0x0387, 1},
		{0x0483, 0x0486, 1},
		{0x0591, 0x05A1, 1},
		{0x05A3, 0x05B9, 1},
		{0x05BB, 0x05BD, 1},
		{0x05BF, 0x05BF, 1},
		{0x05C1, 0x05C2, 1},
		{0x05C4, 0x0640, 0x0640 - 0x05C4},
		{0x064B, 0x0652, 1},
		{0x0660, 0x0669, 1},
		{0x0670, 0x0670, 1},
		{0x06D6, 0x06DC, 1},
		{0x06DD, 0x06DF, 1},
		{0x06E0, 0x06E4, 1},
		{0x06E7, 0x06E8, 1},
		{0x06EA, 0x06ED, 1},
		{0x06F0, 0x06F9, 1},
		{0x0901, 0x0903, 1},
		{0x093C, 0x093C, 1},
		{0x093E, 0x094C, 1},
		{0x094D, 0x094D, 1},
		{0x0951, 0x0954, 1},
		{0x0962, 0x0963, 1},
		{0x0966, 0x096F, 1},
		{0x0981, 0x0983, 1},
		{0x09BC, 0x09BC, 1},
		{0x09BE, 0x09BF, 1},
		{0x09C0, 0x09C4, 1},
		{0x09C7, 0x09C8, 1},
		{0x09CB, 0x09CD, 1},
		{0x09D7, 0x09D7, 1},
		{0x09E2, 0x09E3, 1},
		{0x09E6, 0x09EF, 1},
		{0x0A02, 0x0A3C, 0x3A},
		{0x0A3E, 0x0A3F, 1},
		{0x0A40, 0x0A42, 1},
		{0x0A47, 0x0A48, 1},
		{0x0A4B, 0x0A4D, 1},
		{0x0A66, 0x0A6F, 1},
		{0x0A70, 0x0A71, 1},
		{0x0A81, 0x0A83, 1},
		{0x0ABC, 0x0ABC, 1},
		{0x0ABE, 0x0AC5, 1},
		{0x0AC7, 0x0AC9, 1},
		{0x0ACB, 0x0ACD, 1},
		{0x0AE6, 0x0AEF, 1},
		{0x0B01, 0x0B03, 1},
		{0x0B3C, 0x0B3C, 1},
		{0x0B3E, 0x0B43, 1},
		{0x0B47, 0x0B48, 1},
		{0x0B4B, 0x0B4D, 1},
		{0x0B56, 0x0B57, 1},
		{0x0B66, 0x0B6F, 1},
		{0x0B82, 0x0B83, 1},
		{0x0BBE, 0x0BC2, 1},
		{0x0BC6, 0x0BC8, 1},
		{0x0BCA, 0x0BCD, 1},
		{0x0BD7, 0x0BD7, 1},
		{0x0BE7, 0x0BEF, 1},
		{0x0C01, 0x0C03, 1},
		{0x0C3E, 0x0C44, 1},
		{0x0C46, 0x0C48, 1},
		{0x0C4A, 0x0C4D, 1},
		{0x0C55, 0x0C56, 1},
		{0x0C66, 0x0C6F, 1},
		{0x0C82, 0x0C83, 1},
		{0x0CBE, 0x0CC4, 1},
		{0x0CC6, 0x0CC8, 1},
		{0x0CCA, 0x0CCD, 1},
		{0x0CD5, 0x0CD6, 1},
		{0x0CE6, 0x0CEF, 1},
		{0x0D02, 0x0D03, 1},
		{0x0D3E, 0x0D43, 1},
		{0x0D46, 0x0D48, 1},
		{0x0D4A, 0x0D4D, 1},
		{0x0D57, 0x0D57, 1},
		{0x0D66, 0x0D6F, 1},
		{0x0E31, 0x0E31, 1},
		{0x0E34, 0x0E3A, 1},
		{0x0E46, 0x0E46, 1},
		{0x0E47, 0x0E4E, 1},
		{0x0E50, 0x0E59, 1},
		{0x0EB1, 0x0EB1, 1},
		{0x0EB4, 0x0EB9, 1},
		{0x0EBB, 0x0EBC, 1},
		{0x0EC6, 0x0EC6, 1},
		{0x0EC8, 0x0ECD, 1},
		{0x0ED0, 0x0ED9, 1},
		{0x0F18, 0x0F19, 1},
		{0x0F20, 0x0F29, 1},
		{0x0F35, 0x0F39, 2},
		{0x0F3E, 0x0F3F, 1},
		{0x0F71, 0x0F84, 1},
		{0x0F86, 0x0F8B, 1},
		{0x0F90, 0x0F95, 1},
		{0x0F97, 0x0F97, 1},
		{0x0F99, 0x0FAD, 1},
		{0x0FB1, 0x0FB7, 1},
		{0x0FB9, 0x0FB9, 1},
		{0x20D0, 0x20DC, 1},
		{0x20E1, 0x3005, 0x3005 - 0x20E1},
		{0x302A, 0x302F, 1},
		{0x3031, 0x3035, 1},
		{0x3099, 0x309A, 1},
		{0x309D, 0x309E, 1},
		{0x30FC, 0x30FE, 1},
	},
}

// From https://github.com/golang/go/blob/7a2689b152785010ee2013fb220a048bfe31e49f/src/encoding/xml/xml.go#L1229-L1234
func isNameByte(c byte) bool {
	return 'A' <= c && c <= 'Z' ||
		'a' <= c && c <= 'z' ||
		'0' <= c && c <= '9' ||
		c == '_' || c == ':' || c == '.' || c == '-'
}

// From https://github.com/golang/go/blob/7a2689b152785010ee2013fb220a048bfe31e49f/src/encoding/xml/xml.go#L1236-L1258
func isName(s []byte) bool {
	if len(s) == 0 {
		return false
	}
	c, n := utf8.DecodeRune(s)
	if c == utf8.RuneError && n == 1 {
		return false
	}
	if !unicode.Is(first, c) {
		return false
	}
	for n < len(s) {
		s = s[n:]
		c, n = utf8.DecodeRune(s)
		if c == utf8.RuneError && n == 1 {
			return false
		}
		if !unicode.Is(first, c) && !unicode.Is(second, c) {
			return false
		}
	}
	return true
}

func (r *Reader) readByte() (byte, error) {
	if r.offset >= len(r.data) {
		return 0, &SyntaxError{Offset: r.offset, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF}
	}
	r.offset++
	return r.data[r.offset-1], nil
}

func (r *Reader) readAttrValue() (string, error) {
	if b := r.peek(); b == '"' || b == '\'' {
		return r.readQuotedAttrValue()
	}

	offset := r.offset

	for {
		b, err := r.readByte()
		if err != nil {
			return "", err
		}

		// https://www.w3.org/TR/REC-html40/intro/sgmltut.html#h-3.2.2
		if 'a' <= b && b <= 'z' || 'A' <= b && b <= 'Z' ||
			'0' <= b && b <= '9' || b == '_' || b == ':' || b == '-' {
			continue
		}

		r.offset--

		return bytesToString(r.data[offset:r.offset]), nil
	}
}

var entity = map[string]rune{
	"lt":   '<',
	"gt":   '>',
	"amp":  '&',
	"apos": '\'',
	"quot": '"',
}

func (r *Reader) readQuotedAttrValue() (string, error) {
	// This is only called from readAttrValue, where we already checked that there is a quote byte available, so
	// no need to check the error
	quote, _ := r.readByte()

	buf := r.scratch[:0]

	for {
		b, err := r.readByte()
		if err != nil {
			return "", err
		}

		switch b {
		case quote:
			return bytesToString(buf), nil
		case '<':
			r.offset--
			return "", &SyntaxError{Offset: r.offset, Message: "unescaped < inside quoted string"}
		case '\r':
			// \r and \r\n must be converted to \n, so we simply treat \r as \n and consume the next \n if any
			_ = r.consume('\n')

			buf = append(buf, '\n')
		case '&':
			escBuf := buf[len(buf):]

			if r.consume('#') {
				b, err := r.readByte()
				if err != nil {
					return "", err
				}

				base := 10
				if b == 'x' {
					base = 16

					b, err = r.readByte()
					if err != nil {
						return "", err
					}
				}

				for '0' <= b && b <= '9' ||
					base == 16 && 'a' <= b && b <= 'f' ||
					base == 16 && 'A' <= b && b <= 'F' {
					escBuf = append(escBuf, b)

					b, err = r.readByte()
					if err != nil {
						return "", err
					}
				}

				if b != ';' {
					return "", &SyntaxError{Offset: r.offset, Message: "expected ';'"}
				}

				n, err := strconv.ParseUint(string(escBuf), base, 64)
				if err != nil || n > unicode.MaxRune {
					return "", &SyntaxError{Offset: r.offset - len(escBuf), Message: "invalid number in escape sequence"}
				}

				buf = append(buf, string(rune(n))...)
			} else {
				offset := r.offset

				name, err := r.readName(true)
				if err != nil {
					return "", err
				}

				b, err = r.readByte()
				if err != nil {
					return "", err
				}

				if b != ';' {
					return "", &SyntaxError{Offset: r.offset, Message: "expected ';'"}
				}

				e, ok := entity[name.Local]
				if !ok {
					return "", &UnsupportedEntityError{Offset: offset}
				}

				buf = append(buf, string(e)...)
			}
		default:
			buf = append(buf, b)
		}
	}
}

func (r *Reader) readName(local bool) (Name, error) {
	offset := r.offset

	b, err := r.readByte()
	if err != nil {
		return Name{}, err
	}

	if b < utf8.RuneSelf && !isNameByte(b) {
		r.offset--
		return Name{}, &SyntaxError{Offset: r.offset, Message: "invalid name character"}
	}

	for i := r.offset; i < len(r.data); i++ {
		b, err := r.readByte()
		if err != nil {
			return Name{}, err
		}

		if b < utf8.RuneSelf && !isNameByte(b) {
			r.offset--
			break
		}
	}

	name := r.data[offset:r.offset]

	if !isName(name) {
		r.offset = offset
		return Name{}, &SyntaxError{Offset: r.offset, Message: "invalid name"}
	}

	if local && bytes.IndexByte(name, ':') != -1 {
		r.offset = offset
		return Name{}, &SyntaxError{Offset: r.offset, Message: "name without namespace expected"}
	}

	return bytesToName(name), nil
}
