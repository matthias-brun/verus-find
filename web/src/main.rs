use leptos::prelude::*;
use syn_verus as syn;
use verus_find_web::VerusFindComponent;

// This file is generated in build.rs and includes the full vstd files so we can search in them.
include!(concat!(env!("OUT_DIR"), "/vstd_files.rs"));

pub fn main() {
    _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();

    let mut files = FILES;
    files.sort();
    let files = files
        .iter()
        .map(|(path, file)| {
            (
                path.to_string(),
                syn::parse_file(file)
                    .map_err(|e| {
                        dbg!(&e.span().start(), &e.span().end());
                        format!("failed to parse file: {}", e)
                    })
                    .unwrap(),
            )
        })
        .collect::<Vec<_>>();

    mount_to_body(|| {
        view! {
            <VerusFindComponent
                files=files
            />
        }
    })
}
