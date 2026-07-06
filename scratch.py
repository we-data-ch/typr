import os
import re

with open('crates/typr-lsp/src/lsp_parser.rs', 'r') as f:
    lines = f.readlines()

def get_section(start_marker, end_marker=None):
    start_idx = -1
    for i, line in enumerate(lines):
        if start_marker in line:
            start_idx = i
            break
    if start_idx == -1: return []
    
    end_idx = len(lines)
    if end_marker:
        for i in range(start_idx + 1, len(lines)):
            if end_marker in lines[i]:
                end_idx = i
                break
    return lines[start_idx:end_idx]

# This is a bit too manual. Instead of a python script, let's just do it directly using standard file writing or ask the user to let me do it step by step.
