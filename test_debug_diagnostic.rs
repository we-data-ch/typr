// Test pour debugger l'extraction de position

fn main() {
    let message = r#"thread 'main' panicked at src/processes/type_checking/function_application.rs:39:28:
Err(  × Type error: Function `+` not defined in this scope.
   ╭─[TypR/main.ty:4:1]
 3 │
 4 │ 1 + true
   · ▲
   · ╰── Not defined in this scope
   ╰────
)"#;

    println!("Message original:");
    println!("{}", message);
    println!("\n---\n");

    // Chercher le pattern
    for (i, line) in message.lines().enumerate() {
        println!("Ligne {}: {:?}", i, line);
        if line.contains("╭─[") {
            println!("  -> TROUVÉ ! Cherchons la position...");
            if let Some(start) = line.find("╭─[") {
                println!("  -> start = {}", start);
                if let Some(end) = line[start..].find(']') {
                    let location = &line[start + 3..start + end];
                    println!("  -> location = '{}'", location);

                    // Parse
                    if let Some(last_colon) = location.rfind(':') {
                        if let Some(second_last_colon) = location[..last_colon].rfind(':') {
                            let line_str = &location[second_last_colon + 1..last_colon];
                            let col_str = &location[last_colon + 1..];
                            println!("  -> line_str = '{}', col_str = '{}'", line_str, col_str);

                            if let (Ok(line_num), Ok(col_num)) =
                                (line_str.parse::<u32>(), col_str.parse::<u32>())
                            {
                                println!("  -> Parsed: line={}, col={}", line_num, col_num);
                                println!(
                                    "  -> LSP (0-based): line={}, col={}",
                                    line_num - 1,
                                    col_num - 1
                                );
                            } else {
                                println!("  -> ERREUR: Impossible de parser les nombres");
                            }
                        }
                    }
                }
            }
        }
    }
}
