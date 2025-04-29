package esixml_test

import (
	"errors"
	"io"
	"math"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/nussjustin/esi/esixml"
)

func TestReader(t *testing.T) {
	// Marker value that can be used to specify that a token ends at the end of the input string
	const endIsEOF = math.MinInt

	testCases := []struct {
		Name   string         `json:"name"`
		Input  string         `json:"input"`
		Tokens []esixml.Token `json:"tokens,omitempty"`
		Error  error          `json:"error,omitempty"`
	}{
		{
			Name:  "empty",
			Input: ``,
		},
		{
			Name:  "opening element",
			Input: `<esi:element>`,
			Tokens: []esixml.Token{
				{
					Type:     esixml.TokenTypeStartElement,
					Position: esixml.Position{End: endIsEOF},
					Name:     esixml.Name{Space: "esi", Local: "element"},
				},
			},
		},
		{
			Name:  "opening element with attributes",
			Input: `<esi:element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4">`,
			Tokens: []esixml.Token{
				{
					Type:     esixml.TokenTypeStartElement,
					Position: esixml.Position{End: endIsEOF},
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 25},
							Name:     esixml.Name{Space: "", Local: "attr1"},
							Value:    "value1",
						},
						{
							Position: esixml.Position{Start: 26, End: 40},
							Name:     esixml.Name{Space: "", Local: "attr2"},
							Value:    "value2",
						},
						{
							Position: esixml.Position{Start: 41, End: 55},
							Name:     esixml.Name{Space: "", Local: "attr3"},
							Value:    "value3",
						},
						{
							Position: esixml.Position{Start: 56, End: 73},
							Name:     esixml.Name{Space: "ns", Local: "attr4"},
							Value:    "value4",
						},
					},
				},
			},
		},
		{
			Name:  "self-closing element",
			Input: `<esi:element/>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Closed:   true,
				},
			},
		},
		{
			Name:  "self-closing element with space after /",
			Input: `<esi:element/ >`,
			Error: &esixml.SyntaxError{Offset: 13, Message: "expected '>'"},
		},
		{
			Name:  "self-closing element with attributes",
			Input: `<esi:element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4"/>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 25},
							Name:     esixml.Name{Space: "", Local: "attr1"},
							Value:    "value1",
						},
						{
							Position: esixml.Position{Start: 26, End: 40},
							Name:     esixml.Name{Space: "", Local: "attr2"},
							Value:    "value2",
						},
						{
							Position: esixml.Position{Start: 41, End: 55},
							Name:     esixml.Name{Space: "", Local: "attr3"},
							Value:    "value3",
						},
						{
							Position: esixml.Position{Start: 56, End: 73},
							Name:     esixml.Name{Space: "ns", Local: "attr4"},
							Value:    "value4",
						},
					},
					Closed: true,
				},
			},
		},
		{
			Name:  "self-closing element with attributes and space after /",
			Input: `<esi:element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4"/ >`,
			Error: &esixml.SyntaxError{Offset: 74, Message: "expected '>'"},
		},
		{
			Name:  "end element",
			Input: `</esi:element>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
				},
			},
		},
		{
			Name:  "end element with attributes",
			Input: `</esi:element attr1=value1>`,
			Error: &esixml.SyntaxError{Offset: 14, Message: "expected '>'"},
		},
		{
			Name:  "end element with space before /",
			Input: `< /esi:element>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`< /esi:element>`),
				},
			},
		},
		{
			Name:  "end element with space after /",
			Input: `</ esi:element>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`</ esi:element>`),
				},
			},
		},
		{
			Name:  "non-esi opening element",
			Input: `<element>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`<element>`),
				},
			},
		},
		{
			Name:  "non-esi opening element with attributes",
			Input: `<element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4">`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`<element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4">`),
				},
			},
		},
		{
			Name:  "non-esi self-closing element",
			Input: `<element/>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`<element/>`),
				},
			},
		},
		{
			Name:  "non-esi self-closing element with attributes",
			Input: `<element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4"/>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`<element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4"/>`),
				},
			},
		},
		{
			Name:  "non-esi self-closing element with attributes and space after /",
			Input: `<element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4"/ >`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`<element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4"/ >`),
				},
			},
		},
		{
			Name:  "non-esi self-closing element with space after /",
			Input: `<element/ >`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`<element/ >`),
				},
			},
		},
		{
			Name:  "non-esi self-closing element with attributes and space after /",
			Input: `<element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4"/ >`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`<element attr1=value1 attr2="value2" attr3='value3' ns:attr4="value4"/ >`),
				},
			},
		},
		{
			Name:  "non-esi end element",
			Input: `</element>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`</element>`),
				},
			},
		},
		{
			Name:  "non-esi end element with attributes",
			Input: `</element attr1=value1>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`</element attr1=value1>`),
				},
			},
		},
		{
			Name:  "spaces after attribute name",
			Input: `<esi:element attr =value>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 24},
							Name:     esixml.Name{Space: "", Local: "attr"},
							Value:    "value",
						},
					},
				},
			},
		},
		{
			Name:  "no equals after attribute name",
			Input: `<esi:element attr>`,
			Error: &esixml.SyntaxError{Offset: 17, Message: "expected '='"},
		},
		{
			Name:  "unquoted attribute value before end",
			Input: `<esi:element attr=value>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 23},
							Name:     esixml.Name{Space: "", Local: "attr"},
							Value:    "value",
						},
					},
				},
			},
		},
		{
			Name:  "unquoted attribute value before self-closing end",
			Input: `<esi:element attr=value/>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 23},
							Name:     esixml.Name{Space: "", Local: "attr"},
							Value:    "value",
						},
					},
					Closed: true,
				},
			},
		},
		{
			Name:  "EOF after attribute name",
			Input: `<esi:element attr`,
			Error: &esixml.SyntaxError{Offset: 17, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF},
		},
		{
			Name:  "EOF after attribute equals",
			Input: `<esi:element attr=`,
			Error: &esixml.SyntaxError{Offset: 18, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF},
		},
		{
			Name:  "EOF in quoted attribute value",
			Input: `<esi:element attr="value`,
			Error: &esixml.SyntaxError{Offset: 24, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF},
		},
		{
			Name:  "EOF in unquoted attribute value",
			Input: `<esi:element attr=value`,
			Error: &esixml.SyntaxError{Offset: 23, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF},
		},
		{
			Name:  "Invalid character at element name start",
			Input: `<^esi:element>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data:     []byte(`<^esi:element>`),
				},
			},
		},
		{
			Name:  "Invalid character in element name",
			Input: `<esi:el^ement>`,
			Error: &esixml.SyntaxError{Offset: 7, Message: "invalid name character"},
		},
		{
			Name:  "Invalid character at attribute name start",
			Input: `<esi:element ^attr=value>`,
			Error: &esixml.SyntaxError{Offset: 13, Message: "invalid name character"},
		},
		{
			Name:  "Invalid character in attribute name",
			Input: `<esi:element at^tr=value>`,
			Error: &esixml.SyntaxError{Offset: 15, Message: "expected '='"},
		},
		{
			Name:  "Invalid element name",
			Input: `<esi:e` + string([]byte{12}) + `lement>`,
			Error: &esixml.SyntaxError{Offset: 6, Message: "invalid name character"},
		},
		{
			Name:  "Invalid attribute name",
			Input: `<esi:element at` + string([]byte{12}) + `tr=value>`,
			Error: &esixml.SyntaxError{Offset: 15, Message: "expected '='"},
		},
		{
			Name:  "Invalid attribute name start",
			Input: `<esi:element -attr=value>`,
			Error: &esixml.SyntaxError{Offset: 13, Message: "invalid name"},
		},
		{
			Name:  "Invalid character after escape sequence",
			Input: `<esi:element attr="&#o;">`,
			Error: &esixml.SyntaxError{Offset: 22, Message: "expected ';'"},
		},
		{
			Name:  "Invalid character after escape sequence number",
			Input: `<esi:element attr="&#1">`,
			Error: &esixml.SyntaxError{Offset: 23, Message: "expected ';'"},
		},
		{
			Name:  "Invalid character after entity",
			Input: `<esi:element attr="&entity!">`,
			Error: &esixml.SyntaxError{Offset: 27, Message: "expected ';'"},
		},
		{
			Name:  "\\r to \\n in attribute value",
			Input: `<esi:element attr="multi` + "\r" + `line">`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 30},
							Name:     esixml.Name{Space: "", Local: "attr"},
							Value:    "multi\nline",
						},
					},
				},
			},
		},
		{
			Name:  "\\r\\n to \\n in attribute value",
			Input: `<esi:element attr="multi` + "\r\n" + `line">`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 31},
							Name:     esixml.Name{Space: "", Local: "attr"},
							Value:    "multi\nline",
						},
					},
				},
			},
		},
		{
			Name:  "duplicate attribute name",
			Input: `<esi:element attr1=value attr2=value2 attr1=value3>`,
			Error: &esixml.DuplicateAttributeError{
				Offset: 38,
				Name:   "attr1",
			},
		},
		{
			Name:  "EOF after escape sequence start",
			Input: `<esi:element attr="&`,
			Error: &esixml.SyntaxError{Offset: 20, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF},
		},
		{
			Name:  "EOF after escape sequence",
			Input: `<esi:element attr="&amp`,
			Error: &esixml.SyntaxError{Offset: 23, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF},
		},
		{
			Name:  "EOF after numeric escape sequence start",
			Input: `<esi:element attr="&#`,
			Error: &esixml.SyntaxError{Offset: 21, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF},
		},
		{
			Name:  "EOF after numeric escape sequence",
			Input: `<esi:element attr="&#1`,
			Error: &esixml.SyntaxError{Offset: 22, Message: "unexpected EOF", Underlying: io.ErrUnexpectedEOF},
		},
		{
			Name:  "entity reference in attribute value",
			Input: `<esi:element attr="a &amp; b">`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 29},
							Name:     esixml.Name{Space: "", Local: "attr"},
							Value:    "a & b",
						},
					},
				},
			},
		},
		{
			Name:  "invalid entity reference in attribute value",
			Input: `<esi:element attr="a &invalid; b">`,
			Error: &esixml.UnsupportedEntityError{Offset: 22},
		},
		{
			Name:  "namespaced entity reference in attribute value",
			Input: `<esi:element attr="a &ns:amp; b">`,
			Error: &esixml.SyntaxError{Offset: 22, Message: "name without namespace expected"},
		},
		{
			Name:  "base10 escaped rune in attribute value",
			Input: `<esi:element attr="does this work&#63;">`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 39},
							Name:     esixml.Name{Space: "", Local: "attr"},
							Value:    "does this work?",
						},
					},
				},
			},
		},
		{
			Name:  "invalid base10 escaped rune in attribute value",
			Input: `<esi:element attr="does this work&#99999999999;">`,
			Error: &esixml.SyntaxError{Offset: 36, Message: "invalid number in escape sequence"},
		},
		{
			Name:  "base16 escaped rune in attribute value",
			Input: `<esi:element attr="does this work&#x3F;">`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: endIsEOF},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 40},
							Name:     esixml.Name{Space: "", Local: "attr"},
							Value:    "does this work?",
						},
					},
				},
			},
		},
		{
			Name:  "invalid base16 escaped rune in attribute value",
			Input: `<esi:element attr="does this work&#xFGF;">`,
			Error: &esixml.SyntaxError{Offset: 38, Message: "expected ';'"},
		},

		{
			Name:  "space before closing >",
			Input: `<esi:element >`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: 14},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
				},
			},
		},
		{
			Name:  "space before closing />",
			Input: `<esi:element />`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: 15},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Closed:   true,
				},
			},
		},
		{
			Name:  "space after attribute name",
			Input: `<esi:element attr1 =value1/>`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: 28},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 26},
							Name:     esixml.Name{Space: "", Local: "attr1"},
							Value:    "value1",
						},
					},
					Closed: true,
				},
			},
		},
		{
			Name:  "line breaks in element",
			Input: "<esi:element\nattr1=value1\r\nattr2=\"value2\"\r/>",
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: 44},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "element"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 13, End: 25},
							Name:     esixml.Name{Space: "", Local: "attr1"},
							Value:    "value1",
						},
						{
							Position: esixml.Position{Start: 27, End: 41},
							Name:     esixml.Name{Space: "", Local: "attr2"},
							Value:    "value2",
						},
					},
					Closed: true,
				},
			},
		},

		{
			Name: "complex",
			Input: `
<header>Header</header>

<esi:include src="https://example.com/1.html" alt="https://bak.example.com/2.html" onerror="continue"/>

<p>Paragraph between</p>

<esi:inline name="URI" fetchable="{yes | no}">
	fragment to be stored within an ESI processor
</esi:inline>

<p>Paragraph between</p>

<esi:choose>
	<esi:when test="$(HTTP_COOKIE{group})=='Advanced'">
		<esi:include src="https://www.example.com/advanced.html"/>
	</esi:when>
	<esi:when test="$(HTTP_COOKIE{group})=='Basic User'">
		<esi:include src="https://www.example.com/basic.html"/>
	</esi:when>
	<esi:otherwise>
		<esi:include src="https://www.example.com/new_user.html"/>
	</esi:otherwise>
</esi:choose>

<p>Paragraph between</p>

<esi:try>
	<esi:attempt>
		<esi:comment text="Include an ad"/>
		<esi:include src="https://www.example.com/ad1.html"/>
	</esi:attempt>
	<esi:except>
		<esi:comment text="Just write some HTML instead"/>
		<a href=www.akamai.com>www.example.com</a>
	</esi:except>
</esi:try>

<p>Paragraph between</p>

<esi:comment text="the following animation will have a 24 hr TTL." />

<p>Paragraph between</p>

<esi:remove>
	<a href="https://www.example.com">www.example.com</a>
</esi:remove>

<p>Paragraph between</p>

<esi:vars>
	<img src="https://www.example.com/$(HTTP_COOKIE{type})/hello.gif"/ >
</esi:vars>

<some"><invalid<xml:<elements that="should be </ignored>

<name:spaced-element></name:spaced-element>

<footer>Footer</footer>
			`,
			Tokens: []esixml.Token{
				{
					Position: esixml.Position{End: 25},
					Type:     esixml.TokenTypeData,
					Data:     []byte("<header>Header</header>\n\n"),
				},
				{
					Position: esixml.Position{Start: 25, End: 128},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "include"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 38, End: 70},
							Name:     esixml.Name{Space: "", Local: "src"},
							Value:    "https://example.com/1.html",
						},
						{
							Position: esixml.Position{Start: 71, End: 107},
							Name:     esixml.Name{Space: "", Local: "alt"},
							Value:    "https://bak.example.com/2.html",
						},
						{
							Position: esixml.Position{Start: 108, End: 126},
							Name:     esixml.Name{Space: "", Local: "onerror"},
							Value:    "continue",
						},
					},
					Closed: true,
				},
				{
					Position: esixml.Position{Start: 128, End: 156},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				{
					Position: esixml.Position{Start: 156, End: 202},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "inline"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 168, End: 178},
							Name:     esixml.Name{Space: "", Local: "name"},
							Value:    "URI",
						},
						{
							Position: esixml.Position{Start: 179, End: 201},
							Name:     esixml.Name{Space: "", Local: "fetchable"},
							Value:    "{yes | no}",
						},
					},
				},
				{
					Position: esixml.Position{Start: 202, End: 250},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\tfragment to be stored within an ESI processor\n"),
				},
				{
					Position: esixml.Position{Start: 250, End: 263},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "inline"},
				},
				{
					Position: esixml.Position{Start: 263, End: 291},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				{
					Position: esixml.Position{Start: 291, End: 303},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "choose"},
				},
				{
					Position: esixml.Position{Start: 303, End: 305},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 305, End: 356},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "when"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 315, End: 355},
							Name:     esixml.Name{Space: "", Local: "test"},
							Value:    "$(HTTP_COOKIE{group})=='Advanced'",
						},
					},
				},
				{
					Position: esixml.Position{Start: 356, End: 359},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t\t"),
				},
				{
					Position: esixml.Position{Start: 359, End: 417},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "include"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 372, End: 415},
							Name:     esixml.Name{Space: "", Local: "src"},
							Value:    "https://www.example.com/advanced.html",
						},
					},
					Closed: true,
				},
				{
					Position: esixml.Position{Start: 417, End: 419},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 419, End: 430},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "when"},
				},
				{
					Position: esixml.Position{Start: 430, End: 432},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 432, End: 485},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "when"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 442, End: 484},
							Name:     esixml.Name{Space: "", Local: "test"},
							Value:    "$(HTTP_COOKIE{group})=='Basic User'",
						},
					},
				},
				{
					Position: esixml.Position{Start: 485, End: 488},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t\t"),
				},
				{
					Position: esixml.Position{Start: 488, End: 543},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "include"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 501, End: 541},
							Name:     esixml.Name{Space: "", Local: "src"},
							Value:    "https://www.example.com/basic.html",
						},
					},
					Closed: true,
				},
				{
					Position: esixml.Position{Start: 543, End: 545},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 545, End: 556},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "when"},
				},
				{
					Position: esixml.Position{Start: 556, End: 558},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 558, End: 573},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "otherwise"},
				},
				{
					Position: esixml.Position{Start: 573, End: 576},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t\t"),
				},
				{
					Position: esixml.Position{Start: 576, End: 634},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "include"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 589, End: 632},
							Name:     esixml.Name{Space: "", Local: "src"},
							Value:    "https://www.example.com/new_user.html",
						},
					},
					Closed: true,
				},
				{
					Position: esixml.Position{Start: 634, End: 636},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 636, End: 652},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "otherwise"},
				},
				{
					Position: esixml.Position{Start: 652, End: 653},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n"),
				},
				{
					Position: esixml.Position{Start: 653, End: 666},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "choose"},
				},
				{
					Position: esixml.Position{Start: 666, End: 694},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				{
					Position: esixml.Position{Start: 694, End: 703},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "try"},
				},
				{
					Position: esixml.Position{Start: 703, End: 705},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 705, End: 718},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "attempt"},
				},
				{
					Position: esixml.Position{Start: 718, End: 721},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t\t"),
				},
				{
					Position: esixml.Position{Start: 721, End: 756},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "comment"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 734, End: 754},
							Name:     esixml.Name{Space: "", Local: "text"},
							Value:    "Include an ad",
						},
					},
					Closed: true,
				},
				{
					Position: esixml.Position{Start: 756, End: 759},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t\t"),
				},
				{
					Position: esixml.Position{Start: 759, End: 812},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "include"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 772, End: 810},
							Name:     esixml.Name{Space: "", Local: "src"},
							Value:    "https://www.example.com/ad1.html",
						},
					},
					Closed: true,
				},
				{
					Position: esixml.Position{Start: 812, End: 814},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 814, End: 828},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "attempt"},
				},
				{
					Position: esixml.Position{Start: 828, End: 830},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t"),
				},
				{
					Position: esixml.Position{Start: 830, End: 842},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "except"},
				},
				{
					Position: esixml.Position{Start: 842, End: 845},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t\t"),
				},
				{
					Position: esixml.Position{Start: 845, End: 895},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "comment"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 858, End: 893},
							Name:     esixml.Name{Space: "", Local: "text"},
							Value:    "Just write some HTML instead",
						},
					},
					Closed: true,
				},
				{
					Position: esixml.Position{Start: 895, End: 942},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t\t<a href=www.akamai.com>www.example.com</a>\n\t"),
				},
				{
					Position: esixml.Position{Start: 942, End: 955},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "except"},
				},
				{
					Position: esixml.Position{Start: 955, End: 956},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n"),
				},
				{
					Position: esixml.Position{Start: 956, End: 966},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "try"},
				},
				{
					Position: esixml.Position{Start: 966, End: 994},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				{
					Position: esixml.Position{Start: 994, End: 1063},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "comment"},
					Attr: []esixml.Attr{
						{
							Position: esixml.Position{Start: 1007, End: 1060},
							Name:     esixml.Name{Space: "", Local: "text"},
							Value:    "the following animation will have a 24 hr TTL.",
						},
					},
					Closed: true,
				},
				{
					Position: esixml.Position{Start: 1063, End: 1091},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				{
					Position: esixml.Position{Start: 1091, End: 1103},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "remove"},
				},
				{
					Position: esixml.Position{Start: 1103, End: 1159},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t<a href=\"https://www.example.com\">www.example.com</a>\n"),
				},
				{
					Position: esixml.Position{Start: 1159, End: 1172},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "remove"},
				},
				{
					Position: esixml.Position{Start: 1172, End: 1200},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				{
					Position: esixml.Position{Start: 1200, End: 1210},
					Type:     esixml.TokenTypeStartElement,
					Name:     esixml.Name{Space: "esi", Local: "vars"},
				},
				{
					Position: esixml.Position{Start: 1210, End: 1281},
					Type:     esixml.TokenTypeData,
					Data:     []byte("\n\t<img src=\"https://www.example.com/$(HTTP_COOKIE{type})/hello.gif\"/ >\n"),
				},
				{
					Position: esixml.Position{Start: 1281, End: 1292},
					Type:     esixml.TokenTypeEndElement,
					Name:     esixml.Name{Space: "esi", Local: "vars"},
				},
				{
					Position: esixml.Position{Start: 1292, End: endIsEOF},
					Type:     esixml.TokenTypeData,
					Data: []byte("\n\n<some\"><invalid<xml:<elements that=\"should be </ignored>\n" +
						"\n<name:spaced-element></name:spaced-element>\n\n<footer>Footer</footer>"),
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.Name, func(t *testing.T) {
			input := strings.TrimSpace(testCase.Input)

			for i, token := range testCase.Tokens {
				if token.Position.End == endIsEOF {
					testCase.Tokens[i].Position.End = len(input)
				}
			}

			var gotTokens []esixml.Token
			var gotErr error

			r := esixml.NewReader([]byte(input))

			for token, err := range r.All {
				if err != nil {
					gotErr = err
					break
				}

				gotTokens = append(gotTokens, token)
			}

			if diff := cmp.Diff(testCase.Tokens, gotTokens); diff != "" {
				t.Errorf("Tokens mismatch (-want +got):\n%s", diff)
			}

			if !errors.Is(gotErr, testCase.Error) {
				t.Errorf("got error %v, want %v", gotErr, testCase.Error)
			}

			if gotErr != nil {
				nextToken, nextErr := r.Next()

				if diff := cmp.Diff(esixml.Token{}, nextToken); diff != "" {
					t.Errorf("calling Next() after loop: Tokens mismatch (-want +got):\n%s", diff)
				}

				if !errors.Is(nextErr, gotErr) {
					t.Errorf("calling Next() after loop: got error %v, want %v", gotErr, gotErr)
				}
			}
		})
	}
}

func BenchmarkReader(b *testing.B) {
	b.ReportAllocs()

	var r esixml.Reader

	data := []byte(strings.TrimSpace(`
<header>Header</header>

<esi:include src="https://example.com/1.html" alt="https://bak.example.com/2.html" onerror="continue"/>

<p>Paragraph between</p>

<esi:inline name="URI" fetchable="{yes | no}"> 
	fragment to be stored within an ESI processor 
</esi:inline>

<p>Paragraph between</p>

<esi:choose> 
	<esi:when test="$(HTTP_COOKIE{group})=='Advanced'"> 
		<esi:include src="https://www.example.com/advanced.html"/> 
	</esi:when> 
	<esi:when test="$(HTTP_COOKIE{group})=='Basic User'">
		<esi:include src="https://www.example.com/basic.html"/>
	</esi:when> 
	<esi:otherwise> 
		<esi:include src="https://www.example.com/new_user.html"/> 
	</esi:otherwise>
</esi:choose>

<p>Paragraph between</p>

<esi:try> 
	<esi:attempt>
		<esi:comment text="Include an ad"/> 
		<esi:include src="https://www.example.com/ad1.html"/> 
	</esi:attempt>
	<esi:except> 
		<esi:comment text="Just write some HTML instead"/> 
		<a href=www.akamai.com>www.example.com</a>
	</esi:except> 
</esi:try>

<p>Paragraph between</p>

<esi:comment text="the following animation will have a 24 hr TTL." />

<p>Paragraph between</p>

<esi:remove> 
	<a href="https://www.example.com">www.example.com</a>
</esi:remove>

<p>Paragraph between</p>

<esi:vars>
	<img src="https://www.example.com/$(HTTP_COOKIE{type})/hello.gif"/ >
</esi:vars>

<some"><invalid<xml:<elements that="should be </ignored>

<name:spaced-element></name:spaced-element>

<footer>Footer</footer>`,
	))

	for b.Loop() {
		r.Reset(data)

		for _, err := range r.All {
			if err != nil {
				b.Fatal(err)
			}
		}
	}
}
