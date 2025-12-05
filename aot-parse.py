#!/usr/bin/env python3
import sys
import re

RE_BREAK = r'__attribute__\(\(musttail\)\) return js_tail_dispatch_table\[\*pc\]\([^)]*\);'
#RE_GOTO = r'__attribute__\(\(musttail\)\) return JS_CallInternal_([a-z_]+)\([^)]*\);'

def parse_dispatch_table(preprocessed_code):
    match = re.search(
        r'js_tail_dispatch_table\s*\[\s*\d+\s*\]\s*=\s*\{(.*?)\[\s*OP_COUNT',
        preprocessed_code,
        re.DOTALL
    )
    assert match, "Could not find js_tail_dispatch_table"
    return [m[1] for m in re.finditer(r'\bjs_OP_(\w+)\s*,', match[1])]

def extract_signature(preprocessed_code):
    pattern = re.compile(r'^\s*(.+\s+JSValue)\s+js_OP_invalid\s*\(([^)]*)\)\s*;', re.MULTILINE)
    match = pattern.search(preprocessed_code)
    assert match, "Could not find js_OP_invalid forward declaration"
    return match[1].strip(), match[2].strip()

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

    body = '\n'.join(lines).rstrip()
    body = re.sub(RE_BREAK + '$', '', body).rstrip()
    assert 'js_tail_dispatch_table' not in body
    return body

def escape_string(s):
    m = {'\\': '\\\\', '"': '\\"', '\n': '\\n', '\t': '\\t', '\\r': '\\r'}
    return ''.join([m.get(c, c) for c in s])

def main():
    preprocessed_code = sys.stdin.read()
    opcodes = parse_dispatch_table(preprocessed_code)
    opcode_bodies = parse_op_functions(preprocessed_code)
    sig_prefix, sig_params = extract_signature(preprocessed_code)
    sig_params_init = re.sub('pc', 'pc_init', sig_params)

    print('const char *aot_sig_prefix = "%s";' % escape_string(sig_prefix))
    print('const char *aot_sig_params = "%s";\n' % escape_string(sig_params_init))
    print('const struct { int needs_pc, done_generator; const char *body; } aot_ops[] = {')

    for i, func in enumerate(opcodes):
        assert func in opcode_bodies, f"function not found: js_OP_{func}"
        body = clean_body(opcode_bodies[func])
        needs_pc = int(re.search(r'\bpc\b', body) is not None)
        done_generator = int(re.search(r'done_generator', body) is not None)
        print(f'  /* {i:3d} {func:20} */ {{ {needs_pc}, {done_generator}, "{escape_string(body)}" }},')

    print('};')

if __name__ == '__main__':
    main()
