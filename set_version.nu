# Usage: nu set_version.nu <new_version>
# Example: nu set_version.nu v0.4.20

def main [new_version: string] {
    let header_file = "playground/src/components/Header.tsx"
    let content = open $header_file
    
    # Replace just the version number (vX.Y.Z pattern) - more specific pattern
    let pattern = 'v[0-9]+\.[0-9]+\.[0-9]+'
    let updated_content = ($content | str replace --regex $pattern $new_version)
    
    $updated_content | save -f $header_file

    print $"Updated version to ($new_version) in ($header_file)"
}
