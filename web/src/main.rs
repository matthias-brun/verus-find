use leptos::*;
use verus_find_web::VerusFindComponent;

const DEMO_FILE: &str = include_str!("../../examples.rs");

pub fn main() {
    _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();
    mount_to_body(|| {
        view! {
            <VerusFindComponent
                file=DEMO_FILE
            />
        }
    })
}
