use leptos::prelude::*;
use verus_syn as syn;
use verus_find::matching;

pub fn token_to_view(tok: matching::fmt::FmtToken) -> AnyView {
    match tok {
        matching::fmt::FmtToken::Newline => view! { <br /> }.into_any(),
        matching::fmt::FmtToken::Text(s, matching::fmt::HLToken(d)) => {
            let class = if d == 0 {
                "".to_string()
            } else {
                format!("hl_match{}", d % 3)
            };
            view! { <span class=class>{s}</span> }.into_any()
        }
    }
}

#[component]
pub fn MatchesView(matches: ReadSignal<Vec<matching::ProcessedMatch>, LocalStorage>) -> impl IntoView {
    view! {
        <div>
            <For
                each=move || matches.get()
                key=|m| m.hash_with_default()
                children=move |m| {
                    view! {
                        <hr />
                        <div>
                            <div style="color:green; font-weight: bold;">
                            {m.loc_line}
                            <a href={format!("https://verus-lang.github.io/verus/verusdoc/src/{}.html#{}", m.file, m.line)} target="_blank">[source]</a>
                            </div>
                            <For
                                each=move || m.source_fmt_tokens.clone()
                                key=|m| m.hash()
                                children=move |tok| token_to_view(tok)
                            ></For>
                        </div><br />
                    }
                }
            >
            </For>
        </div>
    }
}

#[derive(Clone)]
enum SearchExprWhere {
    Pre,
    Post,
    Either,
}

#[component]
pub fn VerusFindComponent(files: Vec<(String, syn::File)>) -> impl IntoView {
    let (search_expr_where, set_search_expr_where) = signal_local(SearchExprWhere::Either);
    let (query_expr, set_query_expr) = signal_local("".to_string());
    let (query_sig, set_query_sig) = signal_local("".to_string());
    let (query, set_query) = signal_local(matching::Query::empty());
    let (matches, set_matches) = signal_local(vec![]);
    let (err_expr_class, set_err_expr_class) = signal_local("".to_string());
    let (err_sig_class, set_err_sig_class) = signal_local("".to_string());

    let form_submit = move |ev: leptos::ev::SubmitEvent| {
        //log!("Form submitted");
        ev.prevent_default();
        set_matches.set(
            get_matches(&files, &query.get())
                .into_iter()
                .map(|m| m.to_processed_match())
                .collect());
    };
    Effect::new(move |_| {
        let query_expr = query_expr.get();
        let query_sig = query_sig.get();
        set_err_expr_class.set("".to_string());
        set_err_sig_class.set("".to_string());
        let expr: Option<syn::Expr> = if query_expr.is_empty() {
            None
        } else {
            match syn::parse_str(&query_expr) {
                Ok(e) => Some(e),
                Err(_) => {
                    set_err_expr_class.set("err".to_string());
                    None
                },
            }
        };
        let sig: Option<syn::Signature> = if query_sig.is_empty() {
            None
        } else {
            match syn::parse_str(&query_sig) {
                Ok(e) => Some(e),
                Err(_) => {
                    set_err_sig_class.set("err".to_string());
                    None
                },
            }
        };

        let query = match search_expr_where.get() {
            SearchExprWhere::Pre => matching::Query::new(None, expr, None, None, sig),
            SearchExprWhere::Post => matching::Query::new(None, None, expr, None, sig),
            SearchExprWhere::Either => matching::Query::new(expr, None, None, None, sig),
        };
        set_query.set(query);
    });
    view! {
        <div>
            <form on:submit=form_submit>
                <fieldset id="group1">
                    <span>Search expression:</span>
                    <input type="text" class={move || err_expr_class.get() }
                        on:input=move |ev| { set_query_expr.set(event_target_value(&ev)); }
                    /><span class="err_msg">{move || if !err_expr_class.get().is_empty() { "Invalid expression" } else { "" } }</span><br /><br />
                    <input type="radio" id="radio1" name="group1" on:input=move |_| set_search_expr_where.set(SearchExprWhere::Pre) />
                    <label for="radio1">In the precondition</label><br />
                    <input type="radio" id="radio2" name="group1" on:input=move |_| set_search_expr_where.set(SearchExprWhere::Post) />
                    <label for="radio2">In the postcondition</label><br />
                    <input type="radio" id="radio3" name="group1" checked=true on:input=move |_| set_search_expr_where.set(SearchExprWhere::Either) />
                    <label for="radio3">In either</label><br />
                </fieldset><br />
                <span>Search signature:</span>
                <input type="text" class={move || err_sig_class.get() }
                    on:input=move |ev| { set_query_sig.set(event_target_value(&ev)); }
                /><span class="err_msg">{move || if !err_sig_class.get().is_empty() { "Invalid signature" } else { "" } }</span><br /><br />
                <button>"Search"</button>
            </form>
            <hr />
            <div>"Result: "{move || view! { <span><b>{format!("{} matches found", matches.get().len())}</b></span> } }<br />
                <MatchesView matches=matches />
            </div>
        </div>
    }
}

fn get_matches(files: &[(String, syn::File)], query: &matching::Query) -> Vec<matching::Match> {
    //log!("Searching for {:?}", query);
    let matches = files
        .iter()
        .flat_map(|(path, file)| {
            matching::get_matches_file(file, query, path)
        })
    .collect();
    matches
}
