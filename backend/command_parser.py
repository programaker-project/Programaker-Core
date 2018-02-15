from collections import namedtuple
from xml.dom.minidom import parseString as parse_xml_string

Program = namedtuple('Program', 'variables ast')
Block = namedtuple('Block', 'type fields inner_program')
Field = namedtuple('Field', 'type value')

def _parse_variables(tree):
    return {}

def _parse_block(block):
    block_type = block.getAttribute('type')
    fields = []
    inner_program = []
    for child in block.childNodes:
        if child.attributes is None:
            continue

        elif child.tagName == 'value':
            field_type = child.getElementsByTagName('shadow')[0].getAttribute('type')
            field_value = child.getElementsByTagName('field')[0].firstChild.nodeValue
            fields.append(Field(field_type, field_value))

        elif child.tagName == 'statement':
            inner_program = _parse_nested_ast(child)

    return Block(block_type, fields, inner_program)

def _parse_next(root):
    return _parse_nested_ast(root)

def _parse_nested_ast(tree):
    return _parse_ast_from_block(tree.getElementsByTagName('block')[0])

def _parse_ast_with_alternatives(tree):
    alternatives = []
    for child in tree.childNodes:
        if child.attributes is not None and child.tagName == 'block':
            alternatives.append(_parse_ast_from_block(child))
    return alternatives

def _parse_ast_from_block(root):
    elements = [_parse_block(root)]
    for child in root.childNodes:
        if child.attributes is not None and (child.tagName == 'next'):
            elements += _parse_next(child)
    return elements


def parse(commands):
    tree = parse_xml_string(commands)

    variables = _parse_variables(tree)
    ast = _parse_ast_with_alternatives(tree.firstChild)

    return Program(variables, ast)
