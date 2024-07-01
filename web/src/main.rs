use leptos::*;
use verus_find_web::VerusFindComponent;

const FILES: [(&str, &str); 55] = [
    ("vstd/vstd.rs", include_str!("/st/verus/verus/source/vstd/vstd.rs")),
    ("vstd/arithmetic/mod.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/mod.rs")),
    ("vstd/arithmetic/internals/mod.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/internals/mod.rs")),
    ("vstd/arithmetic/internals/div_internals_nonlinear.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/internals/div_internals_nonlinear.rs")),
    ("vstd/arithmetic/internals/general_internals.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/internals/general_internals.rs")),
    ("vstd/arithmetic/internals/mod_internals_nonlinear.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/internals/mod_internals_nonlinear.rs")),
    ("vstd/arithmetic/internals/mul_internals_nonlinear.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/internals/mul_internals_nonlinear.rs")),
    ("vstd/arithmetic/internals/div_internals.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/internals/div_internals.rs")),
    ("vstd/arithmetic/internals/mod_internals.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/internals/mod_internals.rs")),
    ("vstd/arithmetic/internals/mul_internals.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/internals/mul_internals.rs")),
    ("vstd/arithmetic/div_mod.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/div_mod.rs")),
    ("vstd/arithmetic/logarithm.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/logarithm.rs")),
    ("vstd/arithmetic/mul.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/mul.rs")),
    ("vstd/arithmetic/power.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/power.rs")),
    ("vstd/arithmetic/power2.rs", include_str!("/st/verus/verus/source/vstd/arithmetic/power2.rs")),
    ("vstd/array.rs", include_str!("/st/verus/verus/source/vstd/array.rs")),
    ("vstd/atomic.rs", include_str!("/st/verus/verus/source/vstd/atomic.rs")),
    ("vstd/atomic_ghost.rs", include_str!("/st/verus/verus/source/vstd/atomic_ghost.rs")),
    ("vstd/bits.rs", include_str!("/st/verus/verus/source/vstd/bits.rs")),
    ("vstd/bytes.rs", include_str!("/st/verus/verus/source/vstd/bytes.rs")),
    ("vstd/calc_macro.rs", include_str!("/st/verus/verus/source/vstd/calc_macro.rs")),
    ("vstd/cell.rs", include_str!("/st/verus/verus/source/vstd/cell.rs")),
    ("vstd/function.rs", include_str!("/st/verus/verus/source/vstd/function.rs")),
    ("vstd/invariant.rs", include_str!("/st/verus/verus/source/vstd/invariant.rs")),
    ("vstd/layout.rs", include_str!("/st/verus/verus/source/vstd/layout.rs")),
    ("vstd/map.rs", include_str!("/st/verus/verus/source/vstd/map.rs")),
    ("vstd/map_lib.rs", include_str!("/st/verus/verus/source/vstd/map_lib.rs")),
    ("vstd/math.rs", include_str!("/st/verus/verus/source/vstd/math.rs")),
    ("vstd/modes.rs", include_str!("/st/verus/verus/source/vstd/modes.rs")),
    ("vstd/multiset.rs", include_str!("/st/verus/verus/source/vstd/multiset.rs")),
    ("vstd/pcm.rs", include_str!("/st/verus/verus/source/vstd/pcm.rs")),
    ("vstd/pcm_lib.rs", include_str!("/st/verus/verus/source/vstd/pcm_lib.rs")),
    ("vstd/pervasive.rs", include_str!("/st/verus/verus/source/vstd/pervasive.rs")),
    ("vstd/raw_ptr.rs", include_str!("/st/verus/verus/source/vstd/raw_ptr.rs")),
    ("vstd/seq.rs", include_str!("/st/verus/verus/source/vstd/seq.rs")),
    ("vstd/seq_lib.rs", include_str!("/st/verus/verus/source/vstd/seq_lib.rs")),
    ("vstd/set.rs", include_str!("/st/verus/verus/source/vstd/set.rs")),
    ("vstd/set_lib.rs", include_str!("/st/verus/verus/source/vstd/set_lib.rs")),
    ("vstd/slice.rs", include_str!("/st/verus/verus/source/vstd/slice.rs")),
    ("vstd/state_machine_internal.rs", include_str!("/st/verus/verus/source/vstd/state_machine_internal.rs")),
    ("vstd/storage_protocol.rs", include_str!("/st/verus/verus/source/vstd/storage_protocol.rs")),
    ("vstd/string.rs", include_str!("/st/verus/verus/source/vstd/string.rs")),
    ("vstd/view.rs", include_str!("/st/verus/verus/source/vstd/view.rs")),
    ("vstd/relations.rs", include_str!("/st/verus/verus/source/vstd/relations.rs")),
    ("vstd/std_specs/mod.rs", include_str!("/st/verus/verus/source/vstd/std_specs/mod.rs")),
    ("vstd/std_specs/atomic.rs", include_str!("/st/verus/verus/source/vstd/std_specs/atomic.rs")),
    ("vstd/std_specs/bits.rs", include_str!("/st/verus/verus/source/vstd/std_specs/bits.rs")),
    ("vstd/std_specs/clone.rs", include_str!("/st/verus/verus/source/vstd/std_specs/clone.rs")),
    ("vstd/std_specs/control_flow.rs", include_str!("/st/verus/verus/source/vstd/std_specs/control_flow.rs")),
    ("vstd/std_specs/core.rs", include_str!("/st/verus/verus/source/vstd/std_specs/core.rs")),
    ("vstd/std_specs/num.rs", include_str!("/st/verus/verus/source/vstd/std_specs/num.rs")),
    ("vstd/std_specs/option.rs", include_str!("/st/verus/verus/source/vstd/std_specs/option.rs")),
    ("vstd/std_specs/range.rs", include_str!("/st/verus/verus/source/vstd/std_specs/range.rs")),
    ("vstd/std_specs/result.rs", include_str!("/st/verus/verus/source/vstd/std_specs/result.rs")),
    ("vstd/prelude.rs", include_str!("/st/verus/verus/source/vstd/prelude.rs")),
];

pub fn main() {
    _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();
    mount_to_body(|| {
        view! {
            <VerusFindComponent
                files=&FILES
            />
        }
    })
}
