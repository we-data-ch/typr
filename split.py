import sys
from pathlib import Path
import re

def main():
    mod_path = Path("crates/typr-lsp/src/handlers/mod.rs")
    lines = mod_path.read_text().splitlines()

    import_lines = []
    rest_lines = []
    
    for i, line in enumerate(lines):
        if line.startswith("/// A resolved hover result:"):
            rest_lines = lines[i:]
            break
        import_lines.append(line)
        
    common_imports = "\n".join(import_lines) + "\nuse super::*;\n"
    
    files = {
        "document": [],
        "hover": [],
        "goto_definition": [],
        "signature_help": [],
        "rename": [],
        "utils": [],
        "completions": []
    }
    
    current_file = "document"
    
    i = 0
    while i < len(rest_lines):
        line = rest_lines[i]
        
        # Determine if we need to switch current_file
        if "// ── public entry-point" in line:
            current_file = "hover"
        elif "// ── definition lookup" in line:
            current_file = "goto_definition"
        elif "pub fn offset_to_position(" in line:
            # Special case for this fn which is right before signature help
            current_file = "utils"
        elif "// ── signature help" in line:
            current_file = "signature_help"
        elif "// ── rename / references (occurrence search)" in line:
            current_file = "rename"
        elif "// ── word extraction" in line:
            current_file = "utils"
        elif "// ── literal fallback" in line:
            current_file = "utils"
        elif "// ── Markdown type highlighter" in line:
            current_file = "hover"
        elif "// ── AUTOCOMPLETION" in line:
            current_file = "completions"
        elif "fn extract_multiline_prefix(" in line:
            current_file = "utils"
        elif "fn detect_completion_context(" in line:
            current_file = "completions"
        elif "// ── Type inference helpers" in line:
            current_file = "utils"
        
        # Test modules
        if line.startswith("#[cfg(test)]"):
            next_line = rest_lines[i+1] if i+1 < len(rest_lines) else ""
            if "mod offset_to_position_tests" in next_line:
                current_file = "utils"
            elif "mod hover_project_mode_tests" in next_line:
                current_file = "hover"
            elif "mod mod_completion_tests" in next_line:
                current_file = "completions"
            elif "mod signature_help_tests" in next_line:
                current_file = "signature_help"
            elif "mod rename_and_references_tests" in next_line:
                current_file = "rename"
            elif "mod goto_definition_tests" in next_line:
                current_file = "goto_definition"
                
        files[current_file].append(line)
        i += 1

    # Write files
    handlers_dir = mod_path.parent
    for name, content in files.items():
        out_path = handlers_dir / f"{name}.rs"
        out_path.write_text(common_imports + "\n" + "\n".join(content) + "\n")
        print(f"Wrote {out_path} ({len(content)} lines)")
        
    # Write mod.rs
    mod_content = """pub mod completions;
pub mod document;
pub mod goto_definition;
pub mod hover;
pub mod rename;
pub mod signature_help;
pub mod utils;

pub use completions::*;
pub use document::*;
pub use goto_definition::*;
pub use hover::*;
pub use rename::*;
pub use signature_help::*;
pub use utils::*;
"""
    mod_path.write_text(mod_content)
    print("Wrote mod.rs")

if __name__ == "__main__":
    main()
