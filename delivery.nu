# for R runner
#nu release.nu

# for Docker
#cd ../docker
#nu deploy.nu

# for Crate.io
# Change Cargo's version
let data_file = "configs/src/data.toml"
let new_version = (open $data_file | get release_version | $in + 1);
let updated_line = $"version = \"0.4.($new_version)\"";
sed -i $"3c\\($updated_line)" Cargo.toml
open $data_file | update release_version { $in + 1 } | to toml | save -f $data_file

# publish in cargo
git add .
git commit -m $updated_line 
cargo publish
