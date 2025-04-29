package esi_test

import (
	"bytes"
	"errors"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/nussjustin/esi"
	"github.com/nussjustin/esi/esixml"
)

func TestParse(t *testing.T) {
	name := func(local string) esixml.Name { return esixml.Name{Local: local} }
	nsname := func(local string) esixml.Name { return esixml.Name{Space: "esi", Local: local} }

	position := func(start, end int) esixml.Position {
		return esi.Position{Start: start, End: end}
	}

	attr := func(start, end int, name, value string) esixml.Attr {
		return esixml.Attr{
			Position: position(start, end),
			Name:     esixml.Name{Local: name},
			Value:    value,
		}
	}

	testCases := []struct {
		Name  string
		Input string
		Nodes esi.Nodes
		Error error
	}{
		{
			Name: "empty",
		},

		{
			Name:  "attempt outside try",
			Input: `<esi:attempt>something</esi:attempt>`,
			Error: &esi.UnexpectedElementError{
				Position: esi.Position{
					Start: 0,
					End:   13,
				},
				Name: nsname("attempt"),
			},
		},
		{
			Name: "choose with one when",
			Input: `
				<esi:choose>
					<esi:when test="cond1">1</esi:when>
				</esi:choose>
			`,
			Nodes: esi.Nodes{
				&esi.ChooseElement{
					Position: position(0, 71),
					When: []*esi.WhenElement{
						{
							Position: position(18, 53),
							Test:     "cond1",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(41, 42),
									Bytes:    []byte("1"),
								},
							},
						},
					},
					Otherwise: nil,
				},
			},
		},
		{
			Name: "choose with multiple when",
			Input: `
				<esi:choose>
					<esi:when test="cond1">1</esi:when>
					<esi:when test="cond2">2</esi:when>
				</esi:choose>
			`,
			Nodes: esi.Nodes{
				&esi.ChooseElement{
					Position: position(0, 112),
					When: []*esi.WhenElement{
						{
							Position: position(18, 53),
							Test:     "cond1",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(41, 42),
									Bytes:    []byte("1"),
								},
							},
						},
						{
							Position: position(59, 94),
							Test:     "cond2",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(82, 83),
									Bytes:    []byte("2"),
								},
							},
						},
					},
					Otherwise: nil,
				},
			},
		},
		{
			Name: "choose with multiple when and otherwise",
			Input: `
				<esi:choose>
					<esi:when test="cond1">1</esi:when>
					<esi:when test="cond2">2</esi:when>
					<esi:otherwise>otherwise1</esi:otherwise>
				</esi:choose>
			`,
			Nodes: esi.Nodes{
				&esi.ChooseElement{
					Position: position(0, 159),
					When: []*esi.WhenElement{
						{
							Position: position(18, 53),
							Test:     "cond1",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(41, 42),
									Bytes:    []byte("1"),
								},
							},
						},
						{
							Position: position(59, 94),
							Test:     "cond2",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(82, 83),
									Bytes:    []byte("2"),
								},
							},
						},
					},
					Otherwise: &esi.OtherwiseElement{
						Position: position(100, 141),
						Nodes: esi.Nodes{
							&esi.RawData{
								Position: position(115, 125),
								Bytes:    []byte("otherwise1"),
							},
						},
					},
				},
			},
		},
		{
			Name: "choose with multiple when and multiple otherwise",
			Input: `
				<esi:choose>
					<esi:when test="cond1">1</esi:when>
					<esi:when test="cond2">2</esi:when>
					<esi:otherwise>otherwise1</esi:otherwise>
					<esi:otherwise>otherwise2</esi:otherwise>
				</esi:choose>
			`,
			Error: &esi.DuplicateElementError{
				Position: esi.Position{
					Start: 147,
					End:   188,
				},
				Name: nsname("otherwise"),
			},
		},
		{
			Name: "choose with when that is missing test",
			Input: `
				<esi:choose>
					<esi:when>#</esi:when>
				</esi:choose>
			`,
			Error: &esi.MissingAttributeError{
				Position: esi.Position{
					Start: 18,
					End:   28,
				},
				Element:   nsname("when"),
				Attribute: name("test"),
			},
		},
		{
			Name:  "choose without when",
			Input: `<esi:choose></esi:choose>`,
			Error: &esi.MissingElementError{
				Position: esi.Position{
					Start: 12,
					End:   25,
				},
				Name: nsname("when"),
			},
		},
		{
			Name:  "choose self-closed",
			Input: `<esi:choose/>`,
			Error: &esi.EmptyElementError{
				Position: esi.Position{
					Start: 0,
					End:   13,
				},
				Name: nsname("choose"),
			},
		},
		{
			Name: "choose with invalid data",
			Input: `
				<esi:choose>
					SOME
					<esi:when test="cond1">1</esi:when>
					<esi:comment text="invalid"/>
					<esi:when test="cond2">2</esi:when>
					<esi:remove>data</esi:remove>
				</esi:choose>
			`,
			Nodes: esi.Nodes{
				&esi.ChooseElement{
					Position: position(0, 192),
					When: []*esi.WhenElement{
						{
							Position: position(28, 63),
							Test:     "cond1",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(51, 52),
									Bytes:    []byte("1"),
								},
							},
						},
						{
							Position: position(104, 139),
							Test:     "cond2",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(127, 128),
									Bytes:    []byte("2"),
								},
							},
						},
					},
				},
			},
		},
		{
			Name: "choose unclosed",
			Input: `
				<esi:choose>
					<esi:when test="cond1">1</esi:when>
			`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   12,
				},
				Name: nsname("choose"),
			},
		},
		{
			Name: "choose with unmatched end-element",
			Input: `
				<esi:choose>
					<esi:when test="cond1">1</esi:when>
				</esi:try>
			`,
			Error: &esi.UnexpectedEndElementError{
				Position: esi.Position{
					Start: 58,
					End:   68,
				},
				Name:     nsname("try"),
				Expected: nsname("choose"),
			},
		},
		{
			Name: "choose with extra attributes",
			Input: `
				<esi:choose attr1="value1" attr2="value2">
					<esi:when test="cond1" attr3="value3" attr4="value4"></esi:when>
					<esi:otherwise attr5="value5" attr6="value6"></esi:otherwise>
				</esi:choose>
			`,
			Nodes: esi.Nodes{
				&esi.ChooseElement{
					Position: position(0, 197),
					Attr: []esixml.Attr{
						attr(12, 26, "attr1", "value1"),
						attr(27, 41, "attr2", "value2"),
					},
					When: []*esi.WhenElement{
						{
							Position: position(48, 112),
							Attr: []esixml.Attr{
								attr(71, 85, "attr3", "value3"),
								attr(86, 100, "attr4", "value4"),
							},
							Test:  "cond1",
							Nodes: esi.Nodes{},
						},
					},
					Otherwise: &esi.OtherwiseElement{
						Position: position(118, 179),
						Attr: []esixml.Attr{
							attr(133, 147, "attr5", "value5"),
							attr(148, 162, "attr6", "value6"),
						},
						Nodes: esi.Nodes{},
					},
				},
			},
		},
		{
			Name:  "comment",
			Input: `<esi:comment text="some text"/>`,
			Nodes: esi.Nodes{
				&esi.CommentElement{
					Position: position(0, 31),
					Text:     "some text",
				},
			},
		},
		{
			Name:  "comment without text",
			Input: `<esi:comment/>`,
			Error: &esi.MissingAttributeError{
				Position: esi.Position{
					Start: 0,
					End:   14,
				},
				Element:   nsname("comment"),
				Attribute: name("text"),
			},
		},
		{
			Name:  "comment not self-closed",
			Input: `<esi:comment text="some text">`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   30,
				},
				Name: nsname("comment"),
			},
		},
		{
			Name:  "comment end tag",
			Input: `</esi:comment>`,
			Error: &esi.UnexpectedEndElementError{
				Position: esi.Position{
					Start: 0,
					End:   14,
				},
				Name: nsname("comment"),
			},
		},
		{
			Name:  "comment with extra attributes",
			Input: `<esi:comment text="some comment" attr1="value1" attr2="value2"/>`,
			Nodes: esi.Nodes{
				&esi.CommentElement{
					Position: position(0, 64),
					Attr: []esixml.Attr{
						attr(33, 47, "attr1", "value1"),
						attr(48, 62, "attr2", "value2"),
					},
					Text: "some comment",
				},
			},
		},
		{
			Name:  "except outside try",
			Input: `<esi:except>something</esi:except>`,
			Error: &esi.UnexpectedElementError{
				Position: esi.Position{
					Start: 0,
					End:   12,
				},
				Name: nsname("except"),
			},
		},
		{
			Name:  "include",
			Input: `<esi:include src="/test"/>`,
			Nodes: esi.Nodes{
				&esi.IncludeElement{
					Position: position(0, 26),
					Source:   "/test",
				},
			},
		},
		{
			Name:  "include with alt",
			Input: `<esi:include src="/test" alt="/fallback"/>`,
			Nodes: esi.Nodes{
				&esi.IncludeElement{
					Position: position(0, 42),
					Alt:      "/fallback",
					Source:   "/test",
				},
			},
		},
		{
			Name:  "include with alt and onerror",
			Input: `<esi:include src="/test" alt="/fallback" onerror="continue"/>`,
			Nodes: esi.Nodes{
				&esi.IncludeElement{
					Position: position(0, 61),
					Alt:      "/fallback",
					OnError:  esi.ErrorBehaviourContinue,
					Source:   "/test",
				},
			},
		},
		{
			Name:  "include with onerror",
			Input: `<esi:include src="/test" onerror="continue"/>`,
			Nodes: esi.Nodes{
				&esi.IncludeElement{
					Position: position(0, 45),
					OnError:  esi.ErrorBehaviourContinue,
					Source:   "/test",
				},
			},
		},
		{
			Name:  "include with empty onerror",
			Input: `<esi:include src="/test" onerror=""/>`,
			Error: &esi.InvalidAttributeValueError{
				Position: esi.Position{
					Start: 0,
					End:   37,
				},
				Element: nsname("include"),
				Name:    name("onerror"),
				Value:   "",
				Allowed: []string{"continue"},
			},
		},
		{
			Name:  "include with invalid onerror",
			Input: `<esi:include src="/test" onerror="invalid"/>`,
			Error: &esi.InvalidAttributeValueError{
				Position: esi.Position{
					Start: 0,
					End:   44,
				},
				Element: nsname("include"),
				Name:    name("onerror"),
				Value:   "invalid",
				Allowed: []string{"continue"},
			},
		},
		{
			Name:  "include without src",
			Input: `<esi:include/>`,
			Error: &esi.MissingAttributeError{
				Position: esi.Position{
					Start: 0,
					End:   14,
				},
				Element:   nsname("include"),
				Attribute: name("src"),
			},
		},
		{
			Name:  "include not self-closed",
			Input: `<esi:include src="/test">`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   25,
				},
				Name: nsname("include"),
			},
		},
		{
			Name:  "include end tag",
			Input: `</esi:include>`,
			Error: &esi.UnexpectedEndElementError{
				Position: esi.Position{
					Start: 0,
					End:   14,
				},
				Name: nsname("include"),
			},
		},
		{
			Name:  "include with extra attributes",
			Input: `<esi:include src="/test" attr1="value1" attr2="value2"/>`,
			Nodes: esi.Nodes{
				&esi.IncludeElement{
					Position: position(0, 56),
					Attr: []esixml.Attr{
						attr(25, 39, "attr1", "value1"),
						attr(40, 54, "attr2", "value2"),
					},
					Source: "/test",
				},
			},
		},
		{
			Name:  "inline with fetchable=no",
			Input: `<esi:inline name="/extra" fetchable="no">extra <esi:choose> content</esi:inline>`,
			Nodes: esi.Nodes{
				&esi.InlineElement{
					Position:     position(0, 80),
					FragmentName: "/extra",
					Fetchable:    false,
					Data: esi.RawData{
						Position: position(41, 67),
						Bytes:    []byte("extra <esi:choose> content"),
					},
				},
			},
		},
		{
			Name:  "inline with fetchable=yes",
			Input: `<esi:inline name="/extra" fetchable="yes">extra <esi:choose> content</esi:inline>`,
			Nodes: esi.Nodes{
				&esi.InlineElement{
					Position:     position(0, 81),
					FragmentName: "/extra",
					Fetchable:    true,
					Data: esi.RawData{
						Position: position(42, 68),
						Bytes:    []byte("extra <esi:choose> content"),
					},
				},
			},
		},
		{
			Name:  "inline with invalid fetchable",
			Input: `<esi:inline name="/extra" fetchable="invalid">extra content</esi:inline>`,
			Error: &esi.InvalidAttributeValueError{
				Position: esi.Position{
					Start: 0,
					End:   46,
				},
				Element: nsname("inline"),
				Name:    name("fetchable"),
				Value:   "invalid",
				Allowed: []string{"no", "yes"},
			},
		},
		{
			Name:  "inline without fetchable",
			Input: `<esi:inline name="/extra">extra content</esi:inline>`,
			Error: &esi.MissingAttributeError{
				Position: esi.Position{
					Start: 0,
					End:   26,
				},
				Element:   nsname("inline"),
				Attribute: name("fetchable"),
			},
		},
		{
			Name:  "inline without name",
			Input: `<esi:inline fetchable="no">extra content</esi:inline>`,
			Error: &esi.MissingAttributeError{
				Position: esi.Position{
					Start: 0,
					End:   27,
				},
				Element:   nsname("inline"),
				Attribute: name("name"),
			},
		},
		{
			Name:  "inline unclosed",
			Input: `<esi:inline name="/extra" fetchable="yes">something`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   42,
				},
				Name: nsname("inline"),
			},
		},
		{
			Name:  "inline with unmatched end-element",
			Input: `<esi:inline name="/extra" fetchable="yes">something</esi:when>`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   42,
				},
				Name: nsname("inline"),
			},
		},
		{
			Name:  "inline self-closed",
			Input: `<esi:inline name="/extra" fetchable="yes"/>`,
			Error: &esi.EmptyElementError{
				Position: esi.Position{
					Start: 0,
					End:   43,
				},
				Name: nsname("inline"),
			},
		},
		{
			Name:  "inline with extra attributes",
			Input: `<esi:inline name="/test" fetchable="yes" attr1="value1" attr2="value2"></esi:inline>`,
			Nodes: esi.Nodes{
				&esi.InlineElement{
					Position: position(0, 84),
					Attr: []esixml.Attr{
						attr(41, 55, "attr1", "value1"),
						attr(56, 70, "attr2", "value2"),
					},
					FragmentName: "/test",
					Fetchable:    true,
					Data: esi.RawData{
						Position: position(71, 71),
						Bytes:    []byte{},
					},
				},
			},
		},
		{
			Name:  "otherwise outside choose",
			Input: `<esi:otherwise>something</esi:otherwise>`,
			Error: &esi.UnexpectedElementError{
				Position: esi.Position{
					Start: 0,
					End:   15,
				},
				Name: nsname("otherwise"),
			},
		},
		{
			Name:  "remove",
			Input: `<esi:remove>something <esi:comment text="some comment"/></esi:remove>`,
			Nodes: esi.Nodes{
				&esi.RemoveElement{
					Position: position(0, 69),
					Data: esi.RawData{
						Position: position(12, 56),
						Bytes:    []byte(`something <esi:comment text="some comment"/>`),
					},
				},
			},
		},
		{
			Name:  "remove self-closed",
			Input: `<esi:remove/>`,
			Error: &esi.EmptyElementError{
				Position: position(0, 13),
				Name:     nsname("remove"),
			},
		},
		{
			Name:  "remove end tag",
			Input: `</esi:remove>`,
			Error: &esi.UnexpectedEndElementError{
				Position: esi.Position{
					Start: 0,
					End:   13,
				},
				Name: nsname("remove"),
			},
		},
		{
			Name:  "remove unclosed",
			Input: `<esi:remove>something`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   12,
				},
				Name: nsname("remove"),
			},
		},
		{
			Name:  "remove with unmatched end-element",
			Input: `<esi:remove>something</esi:when>`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   12,
				},
				Name: nsname("remove"),
			},
		},
		{
			Name:  "remove with extra attributes",
			Input: `<esi:remove attr1="value1" attr2="value2"></esi:remove>`,
			Nodes: esi.Nodes{
				&esi.RemoveElement{
					Position: position(0, 55),
					Attr: []esixml.Attr{
						attr(12, 26, "attr1", "value1"),
						attr(27, 41, "attr2", "value2"),
					},
					Data: esi.RawData{
						Position: position(42, 42),
						Bytes:    []byte{},
					},
				},
			},
		},
		{
			Name: "try",
			Input: `
				<esi:try>
					<esi:attempt>attempt1</esi:attempt>
					<esi:except>except1</esi:except>
				</esi:try>
			`,
			Nodes: esi.Nodes{
				&esi.TryElement{
					Position: position(0, 103),
					Attempt: &esi.AttemptElement{
						Position: position(15, 50),
						Nodes: esi.Nodes{
							&esi.RawData{
								Position: position(28, 36),
								Bytes:    []byte(`attempt1`),
							},
						},
					},
					Except: &esi.ExceptElement{
						Position: position(56, 88),
						Nodes: esi.Nodes{
							&esi.RawData{
								Position: position(68, 75),
								Bytes:    []byte(`except1`),
							},
						},
					},
				},
			},
		},
		{
			Name: "try without attempt",
			Input: `
				<esi:try>
					<esi:except>except1</esi:except>
				</esi:try>
			`,
			Error: &esi.MissingElementError{
				Position: esi.Position{
					Start: 52,
					End:   62,
				},
				Name: nsname("attempt"),
			},
		},
		{
			Name: "try with multiple attempt",
			Input: `
				<esi:try>
					<esi:attempt>attempt1</esi:attempt>
					<esi:attempt>attempt2</esi:attempt>
					<esi:except>except1</esi:except>
				</esi:try>
			`,
			Error: &esi.DuplicateElementError{
				Position: esi.Position{
					Start: 56,
					End:   91,
				},
				Name: nsname("attempt"),
			},
		},
		{
			Name: "try without except",
			Input: `
				<esi:try>
					<esi:attempt>attempt1</esi:attempt>
				</esi:try>
			`,
			Error: &esi.MissingElementError{
				Position: esi.Position{
					Start: 55,
					End:   65,
				},
				Name: nsname("except"),
			},
		},
		{
			Name: "try with multiple except",
			Input: `
				<esi:try>
					<esi:attempt>attempt1</esi:attempt>
					<esi:except>except1</esi:except>
					<esi:except>except2</esi:except>
				</esi:try>
			`,
			Error: &esi.DuplicateElementError{
				Position: esi.Position{
					Start: 94,
					End:   126,
				},
				Name: nsname("except"),
			},
		},
		{
			Name: "try with invalid data",
			Input: `
				<esi:try>
					SOME
					<esi:attempt>attempt1</esi:attempt>
					<esi:comment text="invalid"/>
					<esi:except>except1</esi:except>
					<esi:remove>data</esi:remove>
				</esi:try>
			`,
			Nodes: esi.Nodes{
				&esi.TryElement{
					Position: position(0, 183),
					Attempt: &esi.AttemptElement{
						Position: position(25, 60),
						Nodes: esi.Nodes{
							&esi.RawData{
								Position: position(38, 46),
								Bytes:    []byte(`attempt1`),
							},
						},
					},
					Except: &esi.ExceptElement{
						Position: position(101, 133),
						Nodes: esi.Nodes{
							&esi.RawData{
								Position: position(113, 120),
								Bytes:    []byte(`except1`),
							},
						},
					},
				},
			},
		},
		{
			Name:  "try unclosed",
			Input: `<esi:try>`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   9,
				},
				Name: nsname("try"),
			},
		},
		{
			Name: "try with unmatched end-element",
			Input: `
				<esi:try>
					<esi:attempt>attempt1</esi:attempt>
					<esi:except>except1</esi:except>
			`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   9,
				},
				Name: nsname("try"),
			},
		},
		{
			Name:  "try without attempt or except",
			Input: `<esi:try></esi:try>`,
			Error: &esi.MissingElementError{
				Position: esi.Position{
					Start: 9,
					End:   19,
				},
				Name: nsname("attempt"),
			},
		},
		{
			Name:  "try self-closed",
			Input: `<esi:try/>`,
			Error: &esi.EmptyElementError{
				Position: esi.Position{
					Start: 0,
					End:   10,
				},
				Name: nsname("try"),
			},
		},
		{
			Name: "try with extra attributes",
			Input: `
				<esi:try attr1="value1" attr2="value2">
					<esi:attempt attr3="value3"></esi:attempt>
					<esi:except attr4="value4"></esi:except>
				</esi:try>
			`,
			Nodes: esi.Nodes{
				&esi.TryElement{
					Position: position(0, 148),
					Attr: []esixml.Attr{
						attr(9, 23, "attr1", "value1"),
						attr(24, 38, "attr2", "value2"),
					},
					Attempt: &esi.AttemptElement{
						Position: position(45, 87),
						Attr: []esixml.Attr{
							attr(58, 72, "attr3", "value3"),
						},
						Nodes: esi.Nodes{},
					},
					Except: &esi.ExceptElement{
						Position: position(93, 133),
						Attr: []esixml.Attr{
							attr(105, 119, "attr4", "value4"),
						},
						Nodes: esi.Nodes{},
					},
				},
			},
		},
		{
			Name:  "vars",
			Input: `<esi:vars>something<esi:comment text="some comment"/></esi:vars>`,
			Nodes: esi.Nodes{
				&esi.VarsElement{
					Position: position(0, 64),
					Nodes: esi.Nodes{
						&esi.RawData{
							Position: position(10, 19),
							Bytes:    []byte(`something`),
						},
						&esi.CommentElement{
							Position: position(19, 53),
							Text:     "some comment",
						},
					},
				},
			},
		},
		{
			Name:  "vars self-closed",
			Input: `<esi:vars/>`,
			Error: &esi.EmptyElementError{
				Position: esi.Position{
					Start: 0,
					End:   11,
				},
				Name: nsname("vars"),
			},
		},
		{
			Name:  "vars end tag",
			Input: `</esi:vars>`,
			Error: &esi.UnexpectedEndElementError{
				Position: esi.Position{
					Start: 0,
					End:   11,
				},
				Name: nsname("vars"),
			},
		},
		{
			Name:  "vars unclosed",
			Input: `<esi:vars>something`,
			Error: &esi.UnclosedElementError{
				Position: esi.Position{
					Start: 0,
					End:   10,
				},
				Name: nsname("vars"),
			},
		},
		{
			Name:  "vars with unmatched end-element",
			Input: `<esi:vars>something</esi:when>`,
			Error: &esi.UnexpectedEndElementError{
				Position: esi.Position{
					Start: 19,
					End:   30,
				},
				Name:     nsname("when"),
				Expected: nsname("vars"),
			},
		},
		{
			Name:  "vars with extra attributes",
			Input: `<esi:vars attr1="value1" attr2="value2"></esi:vars>`,
			Nodes: esi.Nodes{
				&esi.VarsElement{
					Position: position(0, 51),
					Attr: []esixml.Attr{
						attr(10, 24, "attr1", "value1"),
						attr(25, 39, "attr2", "value2"),
					},
					Nodes: esi.Nodes{},
				},
			},
		},
		{
			Name:  "when outside choose",
			Input: `<esi:when test="cond1">1</esi:when>`,
			Error: &esi.UnexpectedElementError{
				Position: esi.Position{
					Start: 0,
					End:   23,
				},
				Name: nsname("when"),
			},
		},
		{
			Name: "complex",
			Input: `
<header>Header</header>

<esi:include src="https://example.com/1.html" alt="https://bak.example.com/2.html" onerror="continue"/>

<p>Paragraph between</p>

<esi:inline name="URI" fetchable="yes"> 
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
		<esi:comment text="Just writef some HTML instead"/> 
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
			Nodes: esi.Nodes{
				&esi.RawData{
					Position: position(0, 25),
					Bytes:    []byte("<header>Header</header>\n\n"),
				},
				&esi.IncludeElement{
					Position: position(25, 128),
					Alt:      "https://bak.example.com/2.html",
					OnError:  esi.ErrorBehaviourContinue,
					Source:   "https://example.com/1.html",
				},
				&esi.RawData{
					Position: position(128, 156),
					Bytes:    []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				&esi.InlineElement{
					Position:     position(156, 258),
					FragmentName: "URI",
					Fetchable:    true,
					Data: esi.RawData{
						Position: position(195, 245),
						Bytes:    []byte(" \n\tfragment to be stored within an ESI processor \n"),
					},
				},
				&esi.RawData{
					Position: position(258, 286),
					Bytes:    []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				&esi.ChooseElement{
					Position: position(286, 668),
					When: []*esi.WhenElement{
						{
							Position: position(301, 428),
							Test:     "$(HTTP_COOKIE{group})=='Advanced'",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(352, 356),
									Bytes:    []byte(" \n\t\t"),
								},
								&esi.IncludeElement{
									Position: position(356, 414),
									Source:   "https://www.example.com/advanced.html",
								},
								&esi.RawData{
									Position: position(414, 417),
									Bytes:    []byte(" \n\t"),
								},
							},
						},
						{
							Position: position(431, 555),
							Test:     "$(HTTP_COOKIE{group})=='Basic User'",
							Nodes: esi.Nodes{
								&esi.RawData{
									Position: position(484, 487),
									Bytes:    []byte("\n\t\t"),
								},
								&esi.IncludeElement{
									Position: position(487, 542),
									Source:   "https://www.example.com/basic.html",
								},
								&esi.RawData{
									Position: position(542, 544),
									Bytes:    []byte("\n\t"),
								},
							},
						},
					},
					Otherwise: &esi.OtherwiseElement{
						Position: position(558, 654),
						Nodes: esi.Nodes{
							&esi.RawData{
								Position: position(573, 577),
								Bytes:    []byte(" \n\t\t"),
							},
							&esi.IncludeElement{
								Position: position(577, 635),
								Source:   "https://www.example.com/new_user.html",
							},
							&esi.RawData{
								Position: position(635, 638),
								Bytes:    []byte(" \n\t"),
							},
						},
					},
				},
				&esi.RawData{
					Position: position(668, 696),
					Bytes:    []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				&esi.TryElement{
					Position: position(696, 975),
					Attempt: &esi.AttemptElement{
						Position: position(708, 833),
						Nodes: esi.Nodes{
							&esi.RawData{
								Position: position(721, 724),
								Bytes:    []byte("\n\t\t"),
							},
							&esi.CommentElement{
								Position: position(724, 759),
								Text:     "Include an ad",
							},
							&esi.RawData{
								Position: position(759, 763),
								Bytes:    []byte(" \n\t\t"),
							},
							&esi.IncludeElement{
								Position: position(763, 816),
								Source:   "https://www.example.com/ad1.html",
							},
							&esi.RawData{
								Position: position(816, 819),
								Bytes:    []byte(" \n\t"),
							},
						},
					},
					Except: &esi.ExceptElement{
						Position: position(835, 963),
						Nodes: esi.Nodes{
							&esi.RawData{
								Position: position(847, 851),
								Bytes:    []byte(" \n\t\t"),
							},
							&esi.CommentElement{
								Position: position(851, 902),
								Text:     "Just writef some HTML instead",
							},
							&esi.RawData{
								Position: position(902, 950),
								Bytes:    []byte(" \n\t\t<a href=www.akamai.com>www.example.com</a>\n\t"),
							},
						},
					},
				},
				&esi.RawData{
					Position: position(975, 1003),
					Bytes:    []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				&esi.CommentElement{
					Position: position(1003, 1072),
					Text:     "the following animation will have a 24 hr TTL.",
				},
				&esi.RawData{
					Position: position(1072, 1100),
					Bytes:    []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				&esi.RemoveElement{
					Position: position(1100, 1182),
					Data: esi.RawData{
						Position: position(1112, 1169),
						Bytes:    []byte(" \n\t<a href=\"https://www.example.com\">www.example.com</a>\n"),
					},
				},
				&esi.RawData{
					Position: position(1182, 1210),
					Bytes:    []byte("\n\n<p>Paragraph between</p>\n\n"),
				},
				&esi.VarsElement{
					Position: position(1210, 1302),
					Nodes: esi.Nodes{
						&esi.RawData{
							Position: position(1220, 1291),
							Bytes:    []byte("\n\t<img src=\"https://www.example.com/$(HTTP_COOKIE{type})/hello.gif\"/ >\n"),
						},
					},
				},
				&esi.RawData{
					Position: position(1302, 1430),
					Bytes: bytes.Join(
						[][]byte{
							[]byte("\n\n<some\"><invalid<xml:<elements that=\"should be </ignored>\n\n"),
							[]byte("<name:spaced-element></name:spaced-element>\n\n<footer>Footer</footer>"),
						},
						nil,
					),
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.Name, func(t *testing.T) {
			input := strings.TrimSpace(testCase.Input)

			nodes, err := esi.Parse([]byte(input))

			if diff := cmp.Diff(testCase.Nodes, nodes); diff != "" {
				t.Errorf("Parse(...): (-want +got):\n%s", diff)
			}

			if !errors.Is(testCase.Error, err) {
				t.Errorf("got error %v, want %v", err, testCase.Error)
			}
		})
	}
}

func BenchmarkParse(b *testing.B) {
	b.ReportAllocs()

	input := []byte(strings.TrimSpace(`
<header>Header</header>

<esi:include src="https://example.com/1.html" alt="https://bak.example.com/2.html" onerror="continue"/>

<p>Paragraph between</p>

<esi:inline name="URI" fetchable="yes"> 
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
		<esi:comment text="Just writef some HTML instead"/> 
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

<footer>Footer</footer>`))

	for b.Loop() {
		if _, err := esi.Parse(input); err != nil {
			b.Fatal(err)
		}
	}
}
