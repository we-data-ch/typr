import sys
from pathlib import Path

def add_pub(filepath, fn_names):
    p = Path(filepath)
    content = p.read_text()
    for name in fn_names:
        content = content.replace(f"fn {name}(", f"pub fn {name}(")
    p.write_text(content)

base = Path("crates/typr-lsp/src/handlers")

add_pub(base / "utils.rs", [
    "extract_multiline_prefix",
    "var_to_completion_item",
    "infer_expression_type",
    "get_first_parameter_type",
    "extract_word_at",
    "infer_literal_type"
])

add_pub(base / "rename.rs", [
    "extract_plain_word_at",
    "is_plain_word_char"
])

print("Patched visibility.")
