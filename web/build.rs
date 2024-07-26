use std::env;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

fn main() {
    let vstd_path = env::var_os("VSTD_PATH").unwrap().into_string().unwrap();
    let vstd_path_len = vstd_path.len();
    let dest_path = Path::new("./src").join("vstd_files.rs");
    let mut out = "".to_string();
    let mut files = vec![];

    for entry in WalkDir::new(vstd_path) {
        let entry = entry.unwrap();
        if entry.file_type().is_file() && entry.path().file_name().unwrap().to_str().unwrap().ends_with("rs") {
            let file_path = entry.path().to_str().unwrap();
            // I think the slicing here can cause issues with unicode but I'll just hope that
            // vstd files won't be named 😈 anytime soon.
            files.push(format!("(\n\"vstd{}\",\ninclude_str!(\"{}\"),\n),", &file_path[vstd_path_len..], &file_path));
        }
    }
    out.push_str(&format!("pub const FILES: [(&str, &str); {}] = [\n", files.len()));
    for f in files {
        out.push_str(&f);
    }
    out.push_str("];");
    fs::write(&dest_path, out).unwrap();
    //println!("cargo::rerun-if-changed=build.rs");
}
