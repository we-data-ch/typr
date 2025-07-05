cargo build --target x86_64-pc-windows-gnu --release
cargo build --target x86_64-unknown-linux-gnu --release

cp -f target/x86_64-pc-windows-gnu/release/typr.exe ../package_r/typr_runner/inst/bin/ 
cp -f target/x86_64-unknown-linux-gnu/release/typr ../package_r/typr_runner/inst/bin/ 
