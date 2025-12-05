#!/usr/bin/env python3

import sys
import re

BRANCH_OPCODES = {
    'if_false',
    'if_true',
    'goto',
    'gosub',
    'ret',
    'with_get_ref',
    'if_false8',
    'if_true8',
    'goto8',
    'goto16',
}

def parse_dispatch_table(preprocessed_code):
    match = re.search(
        r'js_tail_dispatch_table\s*\[\s*\d+\s*\]\s*=\s*\{(.*?)\[\s*OP_COUNT',
        preprocessed_code,
        re.DOTALL
    )
    assert match, "Could not find js_tail_dispatch_table"

    table_content = match.group(1)

    opcodes = []
    for m in re.finditer(r'\bjs_OP_(\w+)\s*,', table_content):
        opcodes.append(m.group(1))

    return opcodes

def extract_signature(preprocessed_code):
    pattern = re.compile(r'^\s*(.+\s+JSValue)\s+js_OP_invalid\s*\(([^)]*)\)\s*;', re.MULTILINE)

    match = pattern.search(preprocessed_code)
    assert match, "Could not find js_OP_invalid forward declaration"

    return match.group(1).strip(), match.group(2).strip()

def parse_op_functions(preprocessed_code):
    opcode_bodies = {}

    pattern = re.compile(r'\bjs_OP_(\w+)\s*\([^)]*\)\s*\{', re.DOTALL)

    for match in pattern.finditer(preprocessed_code):
        func_name = match.group(1)
        brace_start = match.end() - 1

        brace_count = 1
        pos = brace_start + 1
        while pos < len(preprocessed_code) and brace_count > 0:
            if preprocessed_code[pos] == '{':
                brace_count += 1
            elif preprocessed_code[pos] == '}':
                brace_count -= 1
            pos += 1

        opcode_bodies[func_name] = preprocessed_code[brace_start + 1:pos - 1]

    return opcode_bodies

def clean_body(body):
    lines = [line.rstrip() for line in body.split('\n') if line.strip()]

    min_indent = 1000
    for line in lines:
        min_indent = min(min_indent, len(line) - len(line.lstrip()))
    lines = [line[min_indent:] for line in lines]

    labels = []
    for line in lines:
        labels += re.findall(r'goto (\w+)', line)

    for label in set(labels):
        for i, line in enumerate(lines):
            line = re.sub(r'goto %s' % label, 'goto pcN_%s' % label, line)
            line = re.sub(r'\b%s:' % label, 'pcN_%s:' % label, line)
            lines[i] = line

    body = '\n'.join(lines)

    body = re.sub(
        r'( *)(?:__attribute__\(\([^)]+\)\) )?return\s+js_tail_dispatch_table\s*\[\s*\*\s*pc\s*\]\s*\([^)]*\)\s*;',
        r'\1BREAK;',
        body
    )
    body = re.sub(
        r'( *)(?:__attribute__\(\([^)]+\)\) )?return\s+JS_CallInternal_(\w+)\s*\([^)]*\)\s*;',
        r'\1GOTO(\2);',
        body
    )

    return body.strip()

def remove_trailing_break(body):
    lines = body.split('\n')
    if lines and lines[-1] == 'BREAK;':
        return '\n'.join(lines[:-1]).strip()
    return body

def determine_dispatch(opcode_name, body):
    if opcode_name in BRANCH_OPCODES:
        return "'b'", remove_trailing_break(body)

    lines = body.split('\n')
    assert lines

    if opcode_name == 'tail_call_method':
        return "'g'", body

    if lines[-1].endswith('BREAK;'):
        lines[-1] = lines[-1].removesuffix('BREAK;').strip()
        return "0", '\n'.join(lines).rstrip()

    if re.match(r'GOTO\(\w+\);$', lines[-1].strip()):
        return "'g'", body

    print(f"Error: unknown dispatch for {opcode_name}:", file=sys.stderr)
    print(body, file=sys.stderr)
    sys.exit(1)

def escape_string(s):
    result = []
    for c in s:
        if c == '\\':
            result.append('\\\\')
        elif c == '"':
            result.append('\\"')
        elif c == '\n':
            result.append('\\n')
        elif c == '\t':
            result.append('\\t')
        elif c == '\r':
            result.append('\\r')
        else:
            result.append(c)
    return ''.join(result)

def main():
    preprocessed_code = sys.stdin.read()

    opcodes = parse_dispatch_table(preprocessed_code)
    opcode_bodies = parse_op_functions(preprocessed_code)
    sig_prefix, sig_params = extract_signature(preprocessed_code)
    sig_params_init = re.sub('pc', 'pc_init', sig_params)
    signature = f"{sig_prefix} quickomura%d({sig_params_init})"

    print(f'const char *quickomura_signature = "{escape_string(signature)}";\n')
    print('const struct {')
    print('    const char* name;')
    print('    char dispatch;')
    print('    int need_pc;')
    print('    const char* body;')
    print('} quickomura_opcodes[] = {')

    for i, opcode_name in enumerate(opcodes):
        assert opcode_name in opcode_bodies, f"function not found: js_OP_{opcode_name}"
        body = clean_body(opcode_bodies[opcode_name])
        dispatch, body = determine_dispatch(opcode_name, body)
        need_pc = 1 if re.search(r'\b(pc|GOTO)\b', body) else 0
        print(f'    /*{i:3d}*/ {{ "{opcode_name}", {dispatch}, {need_pc}, "{escape_string(body)}" }},')

    print('};')


if __name__ == '__main__':
    main()
