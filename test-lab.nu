#! /usr/bin/env nu
# test-lab.nu — Run all lab/*.ty files through the type-checker
#
# Usage:
#   nu test-lab.nu                          # Test all lab/*.ty files
#   nu test-lab.nu incr                     # Test only files matching "incr"
#   nu test-lab.nu --debug                  # Show full pipeline (typr debug)

def main [
    filter: string = "",
    --debug(-d),
] {
    let lab_dir = "lab"

    if not ($lab_dir | path exists) {
        print $"'($lab_dir)' directory not found"
        exit 1
    }

    let all_files = (ls $lab_dir | where type == file and name ends-with ".ty" | get name)
    let files = if $filter != "" {
        $all_files | where { |f| ($f | str contains $filter) } | sort
    } else {
        $all_files | sort
    }

    if ($files | length) == 0 {
        if $filter != "" {
            print $"'No .ty files matching ($filter) in ($lab_dir)/'"
        } else {
            print $"No .ty files in ($lab_dir)/"
        }
        exit 1
    }

    let n = ($files | length)
    let hdr = ([$"(ansi green_bold)Running ", $n, " tests(ansi reset)"] | str join)
    print $hdr
    print ""

    let results = $files | each { |file|
        let start = (date now | into int)
        if $debug {
            ^typr debug $file
        } else {
            ^typr check $file
        }
        let status = $env.LAST_EXIT_CODE
        let elapsed = (((date now | into int) - $start) / 1_000_000)
        { file: $file, status: $status, time: $elapsed }
    }

    print ""
    print "Results:"
    print "--------"

    let passed = $results | where status == 0
    let failed = $results | where status != 0

    for r in $results {
        let label = if $r.status == 0 { $"(ansi green)PASS(ansi reset)" } else { $"(ansi red)FAIL(ansi reset)" }
        let f = $r.file
        let t = ($r.time | into string)
        let line = ([$"  ($label)  ", $f, "  (", $t, "ms)"] | str join)
        print $line
    }

    print ""
    let total_time = ($results | get time | math sum)
    let pn = ($passed | length | into string)
    let tn = ($results | length | into string)
    let tt = ($total_time | into string)
    let summary = ([$pn, "/", $tn, " passed (", $tt, "ms total)"] | str join)
    print $summary

    if ($failed | length) > 0 {
        print ""
        let fail_hdr = $"(ansi red)Failing tests:(ansi reset)"
        print $fail_hdr
        for r in $failed {
            let f = $r.file
            print $"  - ($f)"
        }
    }
}
