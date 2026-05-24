#! /usr/bin/env nu
# debug.nu — Build, deploy, and run TypR in one command
#
# Usage:
#   nu debug.nu lab/app.ty                  # Build + run once
#   nu debug.nu lab/app.ty --watch          # Watch mode (re-runs on file change)
#   nu debug.nu lab/app.ty --no-copy        # Skip deploy copy
#   nu debug.nu lab/app.ty --test           # Run cargo test instead
#   nu debug.nu --help                      # Show help

def main [
  file?: string = "lab/app.ty",             # TypR source file to run
  --watch(-w),                               # Watch for changes and re-run
  --no-copy,                                 # Skip copying binary to ~/sh/typr
  --test(-t),                                # Run cargo test for a specific test name
  --nocapture,                               # Show test stdout (implies --test)
  --check,                                   # Only type-check, don't run
  --quiet(-q),                               # Less output from cargo build
  --skip-build,                              # Skip cargo build step
  --bin,                                     # Pass extra args to typr binary
  ...rest                                    # Extra args passed to typr or cargo test
] {
  if $watch {
    print $"Watching for changes... (will run: ($file))"
    cargo watch -w crates -s (build_and_run_command $file $no_copy $check $quiet $skip_build $test $nocapture ...$rest)
  } else {
    if not $skip_build {
      do_build $quiet
    }
    if not $no_copy {
      do_deploy
    }
    if $test or $nocapture {
      do_test ...$rest
    } else if $check {
      do_check $file
    } else {
      do_run $file ...$rest
    }
  }
}

def do_build [quiet: bool] {
  if $quiet {
    cargo build -q
  } else {
    print "Building..."
    cargo build
  }
}

def do_deploy [] {
  cp -f target/debug/typr ~/sh/typr
  print $"Deployed to ~/sh/typr"
}

def do_run [file: string, ...rest: string] {
  let extra = if ($rest | length) > 0 { $rest | str join " " } else { "" }
  print $"Running: typr ($file) ($extra)"
  typr $file ...$rest
}

def do_check [file: string] {
  print $"Checking: typr check ($file)"
  typr check $file
}

def do_test [...rest: string] {
  let filter = if ($rest | length) > 0 { $rest | first } else { "" }
  let capture_flag = if $nocapture { "-- --nocapture" } else { "" }
  if $filter != "" {
    print $"Running tests matching: ($filter)"
    cargo test -p typr-core $filter $"--($capture_flag)"
  } else {
    print "Running all typr-core tests..."
    cargo test -p typr-core
  }
}

def build_and_run_command [
  file: string
  no_copy: bool
  check: bool
  quiet: bool
  skip_build: bool
  test: bool
  nocapture: bool
  ...extra: string
] -> string {
  let parts = []
  let parts = if not $skip_build {
    if $quiet { $parts | append "cargo build -q" } else { $parts | append "cargo build" }
  } else { $parts }
  let parts = if not $no_copy and not $skip_build {
    $parts | append "cp target/debug/typr ~/sh/typr"
  } else { $parts }
  let parts = if $test or $nocapture {
    let filter = if ($extra | length) > 0 { $extra | first } else { "" }
    let nc = if $nocapture { "-- --nocapture" } else { "" }
    if $filter != "" {
      $parts | append $"cargo test -p typr-core ($filter) ($nc)"
    } else {
      $parts | append "cargo test -p typr-core"
    }
  } else if $check {
    $parts | append $"typr check ($file)"
  } else {
    let extras = if ($extra | length) > 0 { $extra | str join " " } else { "" }
    $parts | append $"typr ($file) ($extras)"
  }
  $parts | str join "; "
}
